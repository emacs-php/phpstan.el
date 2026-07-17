# Changelog

All notable changes of the `phpstan.el` are documented in this file using the [Keep a Changelog](https://keepachangelog.com/) principles.

## Unreleased

### Added

* **[experimental]** Add new `phpstan-hover.el`.
  * Add `phpstan-hover-mode` minor mode to show PHPStan type info at point without publishing diagnostics to Flycheck/Flymake.
    * Hover support for variables, assignments, function/method/static calls, constants, class constants, `return`, `yield`, and `yield from`.
    * PHPDoc type collection in hover data and display integration when available.
  * Add `phpstan-hover-show-kind-label` custom variable to toggle verbose labels like `return:` / `yield:` in hover text.
  * Add `phpstan-hover-message-prefix` custom variable preset choices, including emoji labels.
* `phpstan-copy-dumped-type` now prefers PHPDoc type from hover data by default, and can copy non-PHPDoc type with a prefix argument (<kbd>C-u</kbd>).
* Add `container` to `phpstan-executable` to run PHPStan with [Apple container](https://github.com/apple/container) on macOS.  It uses the same `phpstan-docker-image` as `docker`.

### Changed

* `phpstan-copy-dumped-type` command now prioritizes `phpstan-hover-mode` data at point before falling back to dumped-type messages.
* `flycheck-phpstan` is now a Flycheck *generic* checker instead of a command checker.
  * The whole command line is built from `phpstan-executable`, so the checker no longer injects an executable into flycheck's `flycheck-phpstan-executable` variable as a side effect of its `:enabled` predicate, and no longer advises `flycheck-finish-checker-process`.
  * `php` is no longer required in `exec-path` to enable the checker.  Previously the dummy `"php"` in `:command` was resolved by Flycheck *before* the `:enabled` predicate ran, so a Docker-only setup could not enable the checker at all.
  * `M-x flycheck-verify-setup` now reports the real PHPStan command and configuration file instead of `"php"`.
* `phpstan-flycheck-auto-set-executable` is obsolete and ignored.
* `flymake-phpstan` now reads PHPStan's JSON output instead of the raw text format, bringing it to parity with `flycheck-phpstan`.
  * Each message shows its identifier and tip (configurable with `flymake-phpstan-ignore-metadata-list` and `flymake-phpstan-metadata-separator`).
  * `phpstan-insert-ignore` and `phpstan-copy-dumped-type` now work from a Flymake session, because the backend refreshes `phpstan--ignorable-errors` and `phpstan--dumped-types`.
  * Progress a container runtime writes to STDERR no longer confuses the parser, and a PHPStan failure that produces no report is surfaced as a warning rather than dropped.

### Fixed

* Fix `phpstan-get-command-args` to keep `:options` in the correct position and pass target arguments correctly when editor mode options are used.
* Fix `phpstan-executable` in the `(STRING . (ARGUMENTS ...))` form dropping the command name, which made the first *argument* run as the program (`("docker" "run" ...)` executed `run`).
* Fix `phpstan-get-command-args` destructively modifying its inputs with `nconc`.  Each call appended the PHPStan arguments onto the caller's own list, growing `phpstan-executable` in the `(STRING . (ARGUMENTS ...))` form on every check, and appending `"--"` to `phpstan-generate-baseline-options` on every `phpstan-generate-baseline`.
* Fix Flycheck getting stuck on a syntax check when PHPStan reported no files to analyse in a modified buffer.  The check now finishes with an empty result instead of never reporting a status.
* Fix the analyzed file being passed to a containerized PHPStan as a host path.  `flycheck-phpstan` reported no errors at all with `(phpstan-executable . docker)`, because PHPStan answered `Path /Users/... does not exist`.  `flymake-phpstan` was affected for unmodified buffers.
* Fix the `--tmp-file` copy being created in the system temporary directory when running containerized, where the container cannot see it.
* Fix the JSON report being ignored when the container runtime prefixes it with progress output on STDERR, which made every check with `(phpstan-executable . container)` report no errors.
* Fix `flycheck-phpstan` silently discarding the fallback warning when PHPStan produced no JSON report, hiding failures such as a broken configuration file behind a clean buffer.
* Fix editor mode detection asking the wrong program for its version.  Only the first element of the command line was probed, which is the container runtime for `(phpstan-executable . docker)` / `container` and `php` for a PHAR without the executable bit — so `docker --version` and `php --version` were parsed as PHPStan versions (`d1c06ef`, `Technologies`) and editor mode was silently disabled for every setup except a directly executable `phpstan`.
* `phpstan-version` and `phpstan-editor-mode-available-p` now take the whole command line, as returned by `phpstan-get-executable-and-args`.  A bare string is still accepted.  `phpstan-version` no longer merges STDERR into the version string, which a container runtime pollutes with its progress report.
* Fix `declare-function` forms for `tramp` that quoted the function name and argument list (and misspelled `tramp` as `tamp`), so the byte compiler warned that `tramp-dissect-file-name` might not be defined at runtime.
* Fix a container run erroring when the project has no configuration file.  `phpstan-normalize-path` was handed the nil from `phpstan-get-config-file` and passed it to `replace-regexp-in-string`; it now returns nil for a nil path, so the command line simply omits `-c`.

### Removed

* Drop support for Emacs 26 and earlier.  All files now require Emacs 27.1, which is also the floor of `php-mode` and `flycheck` themselves, so anything older could not have resolved its dependencies anyway.

## [0.9.0]

### Added

* Add `phpstan-copy-dumped-type` command to copy the nearest dumped type from `PHPStan\dumpType()` or `PHPStan\dumpPhpDocType()` messages
* Add support for PHPStan [Editor Mode](https://phpstan.org/user-guide/editor-mode)

### Changed

* Improved error handling when no JSON response is returned

### Fixed

* Fixed erroneous dependency from `flymake-phpstan` to Flycheck functions

### Removed

* Drop support for Emacs 25.3

## [0.8.2]

### Fixed

* Fix compilation errors on Emacs 30

## [0.8.1]

### Added

* Add `phpstan-enable-remote-experimental` custom variable for activate PHPStan on TRAMP.
* Add support for `php-ts-mode`.
* Add `phpstan-insert-dumptype` command
* Add `phpstan-insert-ignore` command

### Changed

* Refactored `phpstan-check-buffer` command for improved performance.
* Improved logic for detecting phpstan executable.

### Removed

* Drop support for Emacs 24.3.

## [0.7.0]

### Added

* Add `phpstan-analyze-this-file` command
* Add `phpstan.dist.neon` to `phpstan-get-config-file` as PHPStan config files.
* Add `phpstan-pro` command to launch [PHPStan Pro].
* Add `phpstan-find-baseline-file` command.
* Add `phpstan-generate-baseline-file` command.

[PHPStan Pro]: https://phpstan.org/blog/introducing-phpstan-pro

### Changed

* Make flycheck analyze the original file directly instead of temporary files when there are no changes to the file.
* [Internal] Use JSON format for flycheck-phpstan.
  * Support **💡 tips**
