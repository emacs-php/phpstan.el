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
  (defvar phpstan-working-dir nil
    "Path to working directory of PHPStan.

*NOTICE*: This is different from the project root.

STRING
     Absolute path to `phpstan' working directory.

`(root . STRING)'
     Relative path to `phpstan' working directory from project root directory.

NIL
     Use (php-project-get-root-dir) as working directory.")
  (make-variable-buffer-local 'phpstan-working-dir)
  (put 'phpstan-working-dir 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))

;;;###autoload
(progn
  (defvar phpstan-config-file nil
    "Path to project specific configuration file of PHPStan.

STRING
     Absolute path to `phpstan' configuration file.

`(root . STRING)'
     Relative path to `phpstan' configuration file from project root directory.

NIL
     Search phpstan.neon(.dist) in (phpstan-get-working-dir).")
  (make-variable-buffer-local 'phpstan-config-file)
  (put 'phpstan-config-file 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))

;;;###autoload
(progn
  (defvar phpstan-level "0"
    "Rule level of PHPStan.

INTEGER or STRING
     Number of PHPStan rule level.

max
     The highest of PHPStan rule level.")
  (make-variable-buffer-local 'phpstan-level)
  (put 'phpstan-level 'safe-local-variable
       #'(lambda (v) (or (null v)
                         (integerp v)
                         (eq 'max v)
                         (and (stringp v)
                              (string= "max" v)
                              (string-match-p "\\`[0-9]\\'" v))))))

;;;###autoload
(progn
  (defvar phpstan-replace-path-prefix)
  (make-variable-buffer-local 'phpstan-replace-path-prefix)
  (put 'phpstan-replace-path-prefix 'safe-local-variable
       #'(lambda (v) (or (null v) (stringp v)))))

(defconst phpstan-docker-executable "docker")

;; Usually it is defined dynamically by flycheck
(defvar flycheck-phpstan-executable)

;;;###autoload
(progn
  (defvar phpstan-executable nil
    "PHPStan excutable file.

STRING
     Absolute path to `phpstan' executable file.

`docker'
     Use Docker using phpstan/docker-image.

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
                       (or (eq 'docker v) (null v) (stringp v))))))

;; Functions:
(defun phpstan-get-working-dir ()
  "Return path to working directory of PHPStan."
  (if (and phpstan-working-dir (consp phpstan-working-dir) (eq 'root (car phpstan-working-dir)))
      (expand-file-name (cdr phpstan-working-dir) (php-project-get-root-dir))
    (php-project-get-root-dir)))

(defun phpstan-get-config-file ()
  "Return path to phpstan configure file or `NIL'."
  (if phpstan-config-file
      (if (and (consp phpstan-config-file)
               (eq 'root (car phpstan-config-file)))
          ;; Use (php-project-get-root-dir), not phpstan-working-dir.
          (expand-file-name (cdr phpstan-config-file) (php-project-get-root-dir))
        phpstan-config-file)
    (let ((working-directory (phpstan-get-working-dir)))
      (cl-loop for name in '("phpstan.neon" "phpstan.neon.dist")
               for dir  = (locate-dominating-file working-directory name)
               if dir
               return (expand-file-name name dir)))))

(defun phpstan-enabled-and-set-flycheck-variable ()
  "Return path to phpstan configure file, and set buffer execute in side effect."
  (let ((enabled (not (null (or phpstan-working-dir (phpstan-get-config-file))))))
    (prog1 enabled
      (when (and phpstan-flycheck-auto-set-executable
                 (not (and (boundp 'flycheck-phpstan-executable)
                           (symbol-value 'flycheck-phpstan-executable)))
                 (or (eq 'docker phpstan-executable)
                     (and (consp phpstan-executable)
                          (stringp (car phpstan-executable))
                          (listp (cdr phpstan-executable)))))
        (set (make-local-variable 'flycheck-phpstan-executable)
             (if (eq 'docker phpstan-executable)
                 phpstan-docker-executable
               (car phpstan-executable)))))))

(defun phpstan-normalize-path (source-original &optional source)
  "Return normalized source file path to pass by `SOURCE-ORIGINAL' OR `SOURCE'.

If neither `phpstan-replace-path-prefix' nor executable docker is set,
it returns the value of `SOURCE' as it is."
  (let ((root-directory (expand-file-name (php-project-get-root-dir)))
        (prefix
         (cond
          ((not (null phpstan-replace-path-prefix)) phpstan-replace-path-prefix)
          ((eq 'docker phpstan-executable) "/app")
          ((and (consp phpstan-executable)
                (string= "docker" (car phpstan-executable))) "/app"))))
    (if prefix
        (expand-file-name
         (replace-regexp-in-string (concat "\\`" (regexp-quote root-directory))
                                   ""
                                   source-original t t)
         prefix)
      (or source source-original))))

(defun phpstan-get-level ()
  "Return path to phpstan configure file or `NIL'."
  (cond
   ((null phpstan-level) "0")
   ((integerp phpstan-level) (int-to-string phpstan-level))
   ((symbolp phpstan-level) (symbol-name phpstan-level))
   (t phpstan-level)))

(defun phpstan-get-executable ()
  "Return PHPStan excutable file and arguments."
  (cond
   ((eq 'docker phpstan-executable)
    (list "run" "--rm" "-v"
          (concat (expand-file-name (php-project-get-root-dir)) ":/app")
          "phpstan/phpstan"))
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
       ((file-exists-p vendor-phpstan) (list vendor-phpstan))
       ((executable-find "phpstan") (list (executable-find "phpstan")))
       (t (error "PHPStan executable not found")))))))

(defun phpstan-get-command-args ()
  "Return command line argument for PHPStan."
  (let ((executable (phpstan-get-executable))
        (args (list "analyze" "--errorFormat=raw" "--no-progress" "--no-interaction"))
        (path (phpstan-normalize-path (phpstan-get-config-file)))
        (level (phpstan-get-level)))
    (when path
      (setq args (append args (list "-c" path))))
    (when level
      (setq args (append args (list "-l" level))))
    (append executable args)))

;;;###autoload
(when (featurep 'flycheck)
  (flycheck-define-checker phpstan
    "PHP static analyzer based on PHPStan."
    :command ("php" (eval (phpstan-get-command-args))
              (eval (phpstan-normalize-path
                     (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace)
                     (flycheck-save-buffer-to-temp #'flycheck-temp-file-system))))
    :working-directory (lambda (_) (phpstan-get-working-dir))
    :enabled (lambda () (phpstan-enabled-and-set-flycheck-variable))
    :error-patterns
    ((error line-start (1+ (not (any ":"))) ":" line ":" (message) line-end))
    :modes (php-mode)
    :next-checkers (php)))

(provide 'phpstan)
;;; phpstan.el ends here
