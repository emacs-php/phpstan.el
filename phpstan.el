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
(require 'php-project)
(require 'flycheck nil)


;; Variables:

(defgroup phpstan nil
  "Interaface to PHPStan"
  :tag "PHPStan"
  :prefix "phpstan-"
  :group 'tools
  :group 'php
  :link '(url-link :tag "PHPStan" "https://github.com/phpstan/phpstan")
  :link '(url-link :tag "phpstan.el" "https://github.com/emacs-php/phpstan.el"))

(defcustom phpstan-flycheck-auto-set-executable t
  "Set flycheck phpstan-executable automatically."
  :type 'boolean
  :group 'phpstan)

;;;###autoload
(progn
  (defvar phpstan-config-file nil)
  (make-variable-buffer-local 'phpstan-config-file)
  (put 'phpstan-config-file 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))

;;;###autoload
(progn
  (defvar phpstan-level "0")
  (make-variable-buffer-local 'phpstan-level)
  (put 'phpstan-level 'safe-local-variable
       #'(lambda (v) (or (null v)
                         (integerp v)
                         (and (stringp v)
                              (string-match-p "\\`[0-9]\\'" v))))))

;; Usually it is defined dynamically by flycheck
(defvar flycheck-phpstan-executable)

;;;###autoload
(progn
  (defvar phpstan-executable nil
    "PHPStan excutable file.

STRING
     Absolute path to `phpstan' executable file.

`(root . STRING)'
     Relative path to `phpstan' executable file.

`(STRING . (ARGUMENTS ...))'
     Command name and arguments.

NIL
     Auto detect `phpstan' executable file.")
  (make-variable-buffer-local 'phpstan-executable)
  (put 'phpstan-executable 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (or (and (eq 'root (car v)) (stringp (cdr v)))
                             (and (stringp (car v)) (listp (cdr v))))
                       (or (null v) (stringp v))))))

;; Functions:
(defun phpstan-get-config-file ()
  "Return path to phpstan configure file or `NIL'."
  (if phpstan-config-file
      (if (and (consp phpstan-config-file)
               (eq 'root (car phpstan-config-file)))
          (expand-file-name (cdr phpstan-config-file) (php-project-get-root-dir))
        phpstan-config-file)
    (cl-loop for name in '("phpstan.neon" "phpstan.neon.dist")
             for dir  = (locate-dominating-file default-directory name)
             if dir
             return (expand-file-name name dir))))

(defun phpstan-get-config-file-and-set-flycheck-variable ()
  "Return path to phpstan configure file, and set buffer execute in side effect."
  (prog1 (phpstan-get-config-file)
    (when (and phpstan-flycheck-auto-set-executable
               (not (and (boundp 'flycheck-phpstan-executable)
                         (symbol-value 'flycheck-phpstan-executable)))
               (stringp (car phpstan-executable))
               (listp (cdr phpstan-executable)))
      (set (make-local-variable 'flycheck-phpstan-executable)
           (car phpstan-executable)))))

(defun phpstan-get-level ()
  "Return path to phpstan configure file or `NIL'."
  (cond
   ((null phpstan-level) "0")
   ((integerp phpstan-level) (int-to-string phpstan-level))
   (t phpstan-level)))

(defun phpstan-get-executable ()
  "Return PHPStan excutable file."
  (cond
   ((and (consp phpstan-executable)
         (eq 'root (car phpstan-executable)))
    (expand-file-name (cdr phpstan-executable) (php-project-get-root-dir)))
   ((and phpstan-flycheck-auto-set-executable
         (listp phpstan-executable)
         (stringp (car phpstan-executable))
         (listp (cdr phpstan-executable)))
    (cdr phpstan-executable))
   ((null phpstan-executable)
    (let ((vendor-phpstan (expand-file-name "vendor/bin/phpstan"
                                            (php-project-get-root-dir))))
      (cond
       ((file-exists-p vendor-phpstan) vendor-phpstan)
       ((executable-find "phpstan") (executable-find "phpstan"))
       (t (error "PHPStan executable not found")))))))

;;;###autoload
(when (featurep 'flycheck)
  (flycheck-define-checker phpstan
    "PHP static analyzer based on PHPStan."
    :command ("php" (eval (phpstan-get-executable))
              "analyze" "--errorFormat=raw" "--no-progress" "--no-interaction"
              "-c" (eval (phpstan-get-config-file))
              "-l" (eval (phpstan-get-level))
              source)
    :working-directory (lambda (_) (php-project-get-root-dir))
    :enabled (lambda () (phpstan-get-config-file-and-set-flycheck-variable))
    :error-patterns
    ((error line-start (1+ (not (any ":"))) ":" line ":" (message) line-end))
    :modes (php-mode)
    :next-checkers (php)))

(provide 'phpstan)
;;; phpstan.el ends here
