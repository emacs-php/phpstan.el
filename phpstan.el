;;; phpstan.el --- Interface to PHPStan.             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 15 Mar 2018
;; Version: 0.0.1
;; Keywords: tools, php
;; Homepage: https://github.com/emacs-php/phpstan.el
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Static analyze for PHP code using PHPStan.
;; https://github.com/phpstan/phpstan

;;; Code:
(require 'flycheck nil)

;;;###autoload
(when (featurep 'flycheck)
  (flycheck-def-option-var flycheck-phpstan-config nil phpstan-checker
    "Path to the phpstan configuration for current project.

This is passed to the -l option in phpstan.  It is a good idea is
to use a directory-local variable to specify this per-project."
    :type 'file
    :safe (lambda (x)
            (stringp x)
            (file-exists-p x)))

  (flycheck-def-option-var flycheck-phpstan-level "0" phpstan-checker
    "Strictness level phpstan uses to check the sources.

This is passed to the -c option in phpstan.  A good idea is to
use a directory-local variable to specify this per-project."
    :type 'string
    :safe (lambda (x)
            (and (stringp x)
                 (string-match-p "\\`[0-9]+\\'" x))))

  (flycheck-define-checker phpstan-checker
    "PHP static analyzer based on PHPStan."
    :command ("phpstan" 
              "analyze" 
              "--no-progress"
              "--errorFormat=raw" 
              (option "-l" flycheck-phpstan-level)
              (option "-c" flycheck-phpstan-config)
              source)
    :working-directory (lambda (_) (php-project-get-root-dir))
    :enabled (lambda () (locate-dominating-file "phpstan.neon" default-directory))
    :error-patterns
    ((error line-start (1+ (not (any ":"))) ":" line ":" (message) line-end))
    :modes (php-mode)
    :next-checkers (php)))

(provide 'phpstan)
;;; phpstan.el ends here
