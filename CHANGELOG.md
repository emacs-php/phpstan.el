# Changelog

All notable changes of the `phpstan.el` are documented in this file using the [Keep a Changelog](https://keepachangelog.com/) principles.

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
