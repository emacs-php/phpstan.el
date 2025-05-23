# Changelog

All notable changes of the `phpstan.el` are documented in this file using the [Keep a Changelog](https://keepachangelog.com/) principles.

<!-- ## Unreleased -->

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
