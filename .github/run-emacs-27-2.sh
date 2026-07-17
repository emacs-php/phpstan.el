#!/usr/bin/env bash
# Build this package with Emacs 27.2 from nix-emacs-ci.
#
# CI does not cover Emacs 27 (see .github/workflows/test.yml), so this is the
# way to check the floor declared in Package-Requires by hand.
#
# nix-emacs-ci does not expose Emacs 27 on aarch64-darwin, only on
# x86_64-darwin, so on Apple Silicon this runs through Rosetta.  That needs
# `extra-platforms = x86_64-darwin` in /etc/nix/nix.conf.  Reaching the
# prebuilt binaries also needs your user in `trusted-users` there, otherwise
# the daemon silently ignores the substituter below and builds from source.
set -euo pipefail

# Pinned rather than master, so that the Emacs you get today is the Emacs you
# got last time.  nix-emacs-ci pins nixpkgs in its own flake.lock, so following
# master would change the derivation -- and on a platform without prebuilt
# binaries (see below) that means rebuilding Emacs from source.
NIX_EMACS_CI_REV=${NIX_EMACS_CI_REV:-ca88f42462347e8e17859645aa59b156617e98fb}
NIX_EMACS_CI=${NIX_EMACS_CI:-https://github.com/purcell/nix-emacs-ci/archive/${NIX_EMACS_CI_REV}.tar.gz}
EMACS_ATTR=${EMACS_ATTR:-emacs-27-2}

case "$(uname -s)/$(uname -m)" in
  Darwin/arm64) SYSTEM=x86_64-darwin ;;
  Darwin/*)     SYSTEM=x86_64-darwin ;;
  Linux/x86_64) SYSTEM=x86_64-linux ;;
  Linux/aarch64) SYSTEM=aarch64-linux ;;
  *) echo "unsupported host: $(uname -s)/$(uname -m)" >&2; exit 1 ;;
esac

cd "$(dirname "$0")/.."

echo "==> Fetching ${EMACS_ATTR} for ${SYSTEM}"
emacs_path=$(nix-build --no-out-link \
  --option system "${SYSTEM}" \
  --option extra-substituters https://emacs-ci.cachix.org \
  --option extra-trusted-public-keys emacs-ci.cachix.org-1:B5FVOrxhXXrOL0S+tQ7USrhjMT5iOPH+QN9q0NItom4= \
  -A "${EMACS_ATTR}" \
  -E "import (fetchTarball \"${NIX_EMACS_CI}\")")

export PATH="${emacs_path}/bin:${PATH}"
export EMACS="${emacs_path}/bin/emacs"

echo "==> $("${EMACS}" --version | head -1)"

# Stale .elc files silently win over newer .el, which would hide a failure.
find . -maxdepth 1 -name '*.elc' -delete
rm -rf .eask

echo "==> eask install-deps"
eask install-deps

echo "==> eask compile"
eask compile

echo "==> eask test ert"
eask test ert ./test/*-test.el

echo "==> OK"
