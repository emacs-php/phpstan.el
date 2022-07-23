# Changelog

All notable changes of the phpactor.el are documented in this file using the [Keep a Changelog](https://keepachangelog.com/) principles.

## Unreleased

### Added

* Add `phpstan-analyze-this-file` command
* Add `phpstan.dist.neon` to `phpstan-get-config-file` as PHPStan config files.

### Changed

* Make flycheck analyze the original file directly instead of temporary files when there are no changes to the file.
