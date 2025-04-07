;;; phpstan.el --- Interface to PHPStan              -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 15 Mar 2018
;; Version: 0.8.2
;; Keywords: tools, php
;; Homepage: https://github.com/emacs-php/phpstan.el
;; Package-Requires: ((emacs "25.1") (compat "30") (php-mode "1.22.3") (php-runtime "0.2"))
;; License: GPL-3.0-or-later

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
;;
;; ## Instalation
;;
;; You need to get either the local PHP runtime or Docker and prepare for PHPStan.
;; Please read the README for these details.
;; https://github.com/emacs-php/phpstan.el/blob/master/README.org
;;
;; If you are a Flycheck user, install `flycheck-phpstan' package.
;;
;; ## Directory local variables
;;
;; Put the following into .dir-locals.el files on the root directory of project.
;; Each variable can read what is documented by `M-x describe-variables'.
;;
;;     ((nil . ((php-project-root . git)
;;              (phpstan-executable . docker)
;;              (phpstan-working-dir . (root . "path/to/dir"))
;;              (phpstan-config-file . (root . "path/to/dir/phpstan-docker.neon"))
;;              (phpstan-level . 7))))
;;
;; If you want to know the directory variable specification, please refer to
;; M-x info [Emacs > Customization > Variables] or the following web page.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;;

;;; Code:
(require 'cl-lib)
(require 'php-project)
(require 'php-runtime)
(require 'seq)

(eval-when-compile
  (require 'compat nil t)
  (require 'php)
  (require 'json)
  (declare-function 'tramp-dissect-file-name "tramp" '(name &optional nodefault))
  (declare-function 'tramp-file-name-localname "tamp" '(cl-x)))

;; Variables:
(defgroup phpstan nil
  "Interaface to PHPStan."
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

(defcustom phpstan-enable-on-no-config-file t
  "If T, activate config from composer even when `phpstan.neon' is not found."
  :type 'boolean
  :group 'phpstan)

(defcustom phpstan-memory-limit nil
  "Set --memory-limit option."
  :type '(choice (string :tag "A memory limit number in php.ini format.")
                 (const :tag "Not set --memory-limit option" nil))
  :link '(url-link :tag "PHP Manual"
                   "https://www.php.net/manual/ini.core.php#ini.memory-limit")
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'phpstan)

(defcustom phpstan-docker-image "ghcr.io/phpstan/phpstan"
  "Docker image URL or Docker Hub image name or NIL."
  :type '(choice
          (string :tag "URL or image name of Docker Hub.")
          (const :tag "Official Docker container" "ghcr.io/phpstan/phpstan")
          (const :tag "No specify Docker image" nil))
  :link '(url-link :tag "PHPStan Documentation" "https://phpstan.org/user-guide/docker")
  :link '(url-link :tag "GitHub Container Registry"
                   "https://github.com/orgs/phpstan/packages/container/package/phpstan")
  :safe (lambda (v) (or (null v) (stringp v)))
  :group 'phpstan)

(defcustom phpstan-use-xdebug-option nil
  "Set --xdebug option."
  :type '(choice (const :tag "Set --xdebug option dynamically" auto)
                 (const :tag "Add --xdebug option" t)
                 (const :tag "No --xdebug option" nil))
  :safe #'symbolp
  :group 'phpstan)

(defcustom phpstan-generate-baseline-options '("--generate-baseline" "--allow-empty-baseline")
  "Command line options for generating PHPStan baseline."
  :type '(repeat string)
  :safe #'listp
  :group 'phpstan)

(defcustom phpstan-baseline-file "phpstan-baseline.neon"
  "File name of PHPStan baseline file."
  :type 'string
  :safe #'stringp
  :group 'phpstan)

(defcustom phpstan-tip-message-prefix "💡 "
  "Prefix of PHPStan tip message."
  :type 'string
  :safe #'stringp
  :group 'phpstan)

(defcustom phpstan-identifier-prefix "🪪 "
  "Prefix of PHPStan error identifier."
  :type 'string
  :safe #'stringp
  :group 'phpstan)

(defcustom phpstan-enable-remote-experimental nil
  "Enable PHPStan analysis remotely by TRAMP.

When non-nil, PHPStan will be executed on a remote server for code analysis.
This feature is experimental and should be used with caution as it may
have unexpected behaviors or performance implications."
  :type 'boolean
  :safe #'booleanp
  :group 'phpstan)

(defconst phpstan-template-dump-type "\\PHPStan\\dumpType();")
(defconst phpstan-template-dump-phpdoc-type "\\PHPStan\\dumpPhpDocType();")

(defcustom phpstan-intert-dump-type-templates (cons phpstan-template-dump-type
                                             phpstan-template-dump-phpdoc-type)
  "Default template of PHPStan dumpType insertion."
  :type '(cons string string)
  :group 'phpstan)

(defcustom phpstan-disable-buffer-errors nil
  "If non-NIL, don't keep errors per buffer to save memory."
  :type 'boolean
  :group 'phpstan)

(defcustom phpstan-not-ignorable-identifiers '("ignore.parseError")
  "Lists identifiers prohibited from being added to @phpstan-ignore tags."
  :type '(repeat string))

(defvar-local phpstan--use-xdebug-option nil)

(defvar-local phpstan--ignorable-errors '())
(defvar-local phpstan--dumped-types '())

;;;###autoload
(progn
  (defvar-local phpstan-working-dir nil
    "Path to working directory of PHPStan.

*NOTICE*: This is different from the project root.

STRING
     Absolute path to `phpstan' working directory.

`(root . STRING)'
     Relative path to `phpstan' working directory from project root directory.

NIL
     Use (php-project-get-root-dir) as working directory.")
  (put 'phpstan-working-dir 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))

;;;###autoload
(progn
  (defvar-local phpstan-config-file nil
    "Path to project specific configuration file of PHPStan.

STRING
     Absolute path to `phpstan' configuration file.

`(root . STRING)'
     Relative path to `phpstan' configuration file from project root directory.

NIL
     Search phpstan.neon(.dist) in (phpstan-get-working-dir).")
  (put 'phpstan-config-file 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))

;;;###autoload
(progn
  (defvar-local phpstan-autoload-file nil
    "Path to autoload file for PHPStan.

STRING
     Path to `phpstan' autoload file.

`(root . STRING)'
     Relative path to `phpstan' configuration file from project root directory.

NIL
     If `phpstan-enable-on-no-config-file', search \"vendor/autoload.php\"
     in (phpstan-get-working-dir).")
  (put 'phpstan-autoload-file 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))

;;;###autoload
(progn
  (defvar-local phpstan-level nil
    "Rule level of PHPStan.

INTEGER or STRING
     Number of PHPStan rule level.

max
     The highest of PHPStan rule level.

NIL
     Use rule level specified in `phpstan' configuration file.")
  (put 'phpstan-level 'safe-local-variable
       (lambda (v) (or (null v)
                       (integerp v)
                       (eq 'max v)
                       (and (stringp v)
                            (or (string= "max" v)
                                (string-match-p "\\`[0-9]\\'" v)))))))

;;;###autoload
(progn
  (defvar-local phpstan-replace-path-prefix nil)
  (put 'phpstan-replace-path-prefix 'safe-local-variable
       (lambda (v) (or (null v) (stringp v)))))

(defconst phpstan-docker-executable "docker")

;;;###autoload
(progn
  (defvar-local phpstan-executable nil
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
  (put 'phpstan-executable 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (or (and (eq 'root (car v)) (stringp (cdr v)))
                             (and (stringp (car v)) (listp (cdr v))))
                       (or (eq 'docker v) (null v) (stringp v))))))

;; Utilities:
(defun phpstan--plist-to-alist (plist)
  "Convert PLIST to association list."
  (let (alist)
    (while plist
      (push (cons (substring-no-properties (symbol-name (pop plist)) 1) (pop plist)) alist))
    (nreverse alist)))

(defsubst phpstan--current-line ()
  "Return the current buffer line at point.  The first line is 1."
  (line-number-at-pos nil t))

;; Functions:
(defun phpstan-get-working-dir ()
  "Return path to working directory of PHPStan."
  (cond
   ((and phpstan-working-dir (consp phpstan-working-dir) (eq 'root (car phpstan-working-dir)))
    (expand-file-name (cdr phpstan-working-dir) (php-project-get-root-dir)))
   ((stringp phpstan-working-dir) phpstan-working-dir)
   (t (php-project-get-root-dir))))

(defun phpstan-enabled ()
  "Return non-NIL if PHPStan configured or Composer detected."
  (unless (and (not phpstan-enable-remote-experimental)
               (file-remote-p default-directory)) ;; Not support remote filesystem by default
    (or (phpstan-get-config-file)
        (phpstan-get-autoload-file)
        (and phpstan-enable-on-no-config-file
             (php-project-get-root-dir)))))

(defun phpstan-get-config-file ()
  "Return path to phpstan configure file or NIL."
  (if phpstan-config-file
      (if (and (consp phpstan-config-file)
               (eq 'root (car phpstan-config-file)))
          ;; Use (php-project-get-root-dir), not phpstan-working-dir.
          (expand-file-name (cdr phpstan-config-file) (php-project-get-root-dir))
        phpstan-config-file)
    (let ((working-directory (phpstan-get-working-dir)))
      (when working-directory
        (cl-loop for name in '("phpstan.neon" "phpstan.neon.dist" "phpstan.dist.neon")
                 for dir = (locate-dominating-file working-directory name)
                 if dir
                 return (expand-file-name name dir))))))

(defun phpstan-get-autoload-file ()
  "Return path to autoload file or NIL."
  (when phpstan-autoload-file
    (if (and (consp phpstan-autoload-file)
             (eq 'root (car phpstan-autoload-file)))
        (expand-file-name (cdr phpstan-autoload-file) (php-project-get-root-dir))
      phpstan-autoload-file)))

(defun phpstan-normalize-path (source-original &optional source)
  "Return normalized source file path to pass by SOURCE-ORIGINAL or SOURCE.

If neither `phpstan-replace-path-prefix' nor executable docker is set,
it returns the value of `SOURCE' as it is."
  (let ((root-directory (expand-file-name (php-project-get-root-dir)))
        (prefix
         (or phpstan-replace-path-prefix
             (cond
              ((eq 'docker phpstan-executable) "/app")
              ((and (consp phpstan-executable)
                    (string= "docker" (car phpstan-executable)))
               "/app")))))
    (if prefix
        (expand-file-name
         (replace-regexp-in-string (concat "\\`" (regexp-quote root-directory))
                                   ""
                                   source-original t t)
         prefix)
      (or source source-original))))

(defun phpstan-get-level ()
  "Return path to phpstan configure file or NIL."
  (cond
   ((null phpstan-level) nil)
   ((integerp phpstan-level) (int-to-string phpstan-level))
   ((symbolp phpstan-level) (symbol-name phpstan-level))
   (t phpstan-level)))

(defun phpstan-get-memory-limit ()
  "Return --memory-limit value."
  phpstan-memory-limit)

(defun phpstan--parse-json (buffer)
  "Read JSON string from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    ;; Ignore STDERR
    (save-match-data
      (when (search-forward-regexp "^{" nil t)
        (backward-char 1)
        (delete-region (point-min) (point))))
    (if (eval-when-compile (and (fboundp 'json-serialize)
                                (fboundp 'json-parse-buffer)))
        (with-no-warnings
          (json-parse-buffer :object-type 'plist :array-type 'list))
      (let ((json-object-type 'plist) (json-array-type 'list))
        (json-read-object)))))

(defun phpstan--expand-file-name (name)
  "Expand file name by NAME."
  (let ((file (expand-file-name name)))
    (if (file-remote-p name)
        (tramp-file-name-localname (tramp-dissect-file-name name))
      (expand-file-name file))))

;;;###autoload
(defun phpstan-analyze-this-file ()
  "Analyze current buffer-file using PHPStan."
  (interactive)
  (let ((file (phpstan--expand-file-name (or buffer-file-name
                                      (read-file-name "Choose a PHP script: ")))))
    (compile (mapconcat #'shell-quote-argument
                        (phpstan-get-command-args :include-executable t :args (list file) :verbose 1) " "))))

;;;###autoload
(defun phpstan-analyze-file (file)
  "Analyze a PHP script FILE using PHPStan."
  (interactive (list (phpstan--expand-file-name (read-file-name "Choose a PHP script: "))))
  (compile (mapconcat #'shell-quote-argument
                      (phpstan-get-command-args :include-executable t :args (list file) :verbose 1) " ")))

;;;###autoload
(defun phpstan-analyze-project ()
  "Analyze a PHP project using PHPStan."
  (interactive)
  (let ((default-directory (or (php-project-get-root-dir) default-directory)))
    (compile (mapconcat #'shell-quote-argument (phpstan-get-command-args :include-executable t) " "))))

;;;###autoload
(defun phpstan-generate-baseline ()
  "Generate PHPStan baseline file."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory phpstan-baseline-file)
                               (php-project-get-root-dir)
                               default-directory)))
    (compile (mapconcat #'shell-quote-argument
                        (phpstan-get-command-args :include-executable t :options phpstan-generate-baseline-options) " "))))

;;;###autoload
(defun phpstan-find-baseline-file ()
  "Find PHPStan baseline file of current project."
  (interactive)
  (if-let ((path (locate-dominating-file default-directory phpstan-baseline-file)))
      (find-file (expand-file-name phpstan-baseline-file path))
    (user-error "Baseline file not found.  Try running M-x phpstan-generate-baseline")))

;;;###autoload
(defun phpstan-pro ()
  "Analyze current PHP project using PHPStan Pro."
  (interactive)
  (let ((compilation-buffer-name-function (lambda (_) "*PHPStan Pro*"))
        (command (mapconcat #'shell-quote-argument
                            (phpstan-get-command-args :include-executable t :use-pro t) " ")))
    (compile command t)))

(defun phpstan-get-executable-and-args ()
  "Return PHPStan excutable file and arguments."
  (cond
   ((eq 'docker phpstan-executable)
    (list phpstan-docker-executable "run" "--rm" "-v"
          (concat (expand-file-name (php-project-get-root-dir)) ":/app")
          phpstan-docker-image))
   ((and (consp phpstan-executable)
         (eq 'root (car phpstan-executable)))
    (let ((phpstan (expand-file-name (cdr phpstan-executable) (php-project-get-root-dir))))
      (if (file-executable-p phpstan)
          (list phpstan)
        (list php-executable phpstan))))
   ((and (stringp phpstan-executable))
    (unless (file-exists-p phpstan-executable)
      (user-error "File %s is not exists.  Please check `phpstan-executable' variable" phpstan-executable))
    (when (file-directory-p phpstan-executable)
      (user-error "Path %s is a directory.  Please check `phpstan-executable' variable" phpstan-executable))
    (if (file-executable-p phpstan-executable)
        (list phpstan-executable)
      (list php-executable phpstan-executable)))
   ((and phpstan-flycheck-auto-set-executable
         (listp phpstan-executable)
         (stringp (car phpstan-executable))
         (listp (cdr phpstan-executable)))
    (cdr phpstan-executable))
   ((null phpstan-executable)
    (let* ((vendor-phpstan (expand-file-name "vendor/bin/phpstan"
                                             (php-project-get-root-dir)))
           (expanded-vendor-phpstan (phpstan--expand-file-name vendor-phpstan)))
      (cond
       ((file-exists-p vendor-phpstan)
        (if (file-executable-p vendor-phpstan)
            (list expanded-vendor-phpstan)
          (list php-executable expanded-vendor-phpstan)))
       ((executable-find "phpstan") (list (executable-find "phpstan")))
       (t (error "PHPStan executable not found")))))))

(cl-defun phpstan-get-command-args (&key include-executable use-pro args format options config verbose)
  "Return command line argument for PHPStan."
  (let ((executable-and-args (phpstan-get-executable-and-args))
        (config (or config (phpstan-normalize-path (phpstan-get-config-file))))
        (autoload (phpstan-get-autoload-file))
        (memory-limit (phpstan-get-memory-limit))
        (level (phpstan-get-level)))
    (nconc (if include-executable (list (car executable-and-args)) nil)
           (cdr executable-and-args)
           (list "analyze"
                 (format "--error-format=%s" (or format "raw"))
                 "--no-progress" "--no-interaction")
           (and use-pro (list "--pro" "--no-ansi"))
           (and config (list "-c" (phpstan--expand-file-name config)))
           (and autoload (list "-a" autoload))
           (and memory-limit (list "--memory-limit" memory-limit))
           (and level (list "-l" level))
           (cond
            ((null verbose) nil)
            ((memq verbose '(1 t)) (list "-v"))
            ((eq verbose 2) (list "-vv"))
            ((eq verbose 3) (list "-vvv"))
            ((error ":verbose option should be 1, 2, 3 or `t'")))
           (cond
            (phpstan--use-xdebug-option (list phpstan--use-xdebug-option))
            ((eq phpstan-use-xdebug-option 'auto)
             (setq-local phpstan--use-xdebug-option
                         (when (string= "1" (php-runtime-expr "extension_loaded('xdebug')"))
                           "--xdebug"))
             (list phpstan--use-xdebug-option))
            (phpstan-use-xdebug-option (list "--xdebug")))
           options
           (and args (cons "--" args)))))

(defun phpstan-update-ignorebale-errors-from-json-buffer (errors)
  "Update `phpstan--ignorable-errors' variable by ERRORS."
  (let ((identifiers
         (cl-loop for (_ . entry) in errors
                  append (cl-loop for message in (plist-get entry :messages)
                                  if (plist-get message :ignorable)
                                  collect (cons (plist-get message :line)
                                                (plist-get message :identifier))))))
    (setq phpstan--ignorable-errors
          (mapcar (lambda (v) (cons (car v) (mapcar #'cdr (cdr v)))) (seq-group-by #'car identifiers)))))

(defun phpstan-update-dumped-types (errors)
  "Update `phpstan--dumped-types' variable by ERRORS."
  (save-match-data
    (setq phpstan--dumped-types
          (cl-loop for (_ . entry) in errors
                   append (cl-loop for message in (plist-get entry :messages)
                                   for msg = (plist-get message :message)
                                   if (string-match (eval-when-compile (rx bos "Dumped type: ")) msg)
                                   collect (cons (plist-get message :line)
                                                 (substring-no-properties msg (match-end 0))))))))

(defconst phpstan--re-ignore-tag
  (eval-when-compile
    (rx (* (syntax whitespace)) "//" (* (syntax whitespace))
        (group "@phpstan-ignore")
        (* (syntax whitespace))
        (* (not "("))
        (group (? (+ (syntax whitespace) "("))))))

(cl-defun phpstan--check-existing-ignore-tag (&key in-previous)
  "Check existing @phpstan-ignore PHPDoc tag.
If IN-PREVIOUS is NIL, check the previous line for the tag."
  (let ((new-position (if in-previous 'previous-line 'this-line))
        (line-end (line-end-position))
        new-point append)
    (save-excursion
      (save-match-data
        (if (re-search-forward phpstan--re-ignore-tag line-end t)
            (progn
              (setq new-point (match-beginning 2))
              (goto-char new-point)
              (when (eq (char-syntax (char-before)) ?\ )
                (left-char)
                (setq new-point (point)))
              (setq append (not (eq (match-end 1) (match-beginning 2))))
              (cl-values new-position new-point append))
          (if in-previous
              (cl-values nil nil nil)
            (previous-logical-line)
            (beginning-of-line)
            (phpstan--check-existing-ignore-tag :in-previous t)))))))

;;;###autoload
(defun phpstan-insert-ignore (position)
  "Insert an @phpstan-ignore comment at the specified POSITION.

POSITION determines where to insert the comment and can be either `this-line' or
`previous-line'.

- If POSITION is `this-line', the comment is inserted at the end of
  the current line.
- If POSITION is `previous-line', the comment is inserted on a new line above
  the current line."
  (interactive
   (list (if current-prefix-arg 'this-line 'previous-line)))
  (save-restriction
    (widen)
    (let ((identifiers (cl-set-difference (alist-get (phpstan--current-line) phpstan--ignorable-errors) phpstan-not-ignorable-identifiers :test #'equal))
          (padding (if (eq position 'this-line) " " ""))
          new-position new-point append)
      (cl-multiple-value-setq (new-position new-point append) (phpstan--check-existing-ignore-tag :in-previous nil))
      (when new-position
        (setq position new-position))
      (unless (and append (null identifiers))
        (if (not new-point)
            (cond
             ((eq position 'this-line) (end-of-line))
             ((eq position 'previous-line) (progn
                                             (previous-logical-line)
                                             (end-of-line)
                                             (newline-and-indent)))
             ((error "Unexpected position: %s" position)))
          (setq padding "")
          (goto-char new-point))
        (insert (concat padding
                        (if new-position (if append ", " " ") "// @phpstan-ignore ")
                        (string-join identifiers ", ")))))))

;;;###autoload
(defun phpstan-copy-dumped-type ()
  "Copy a dumped PHPStan type."
  (interactive)
  (if phpstan--dumped-types
      (let ((type (if (eq 1 (length phpstan--dumped-types))
                      (cdar phpstan--dumped-types)
                    (let ((linum (line-number-at-pos)))
                      (cdar (seq-sort-by (lambda (elm) (abs (- linum (car elm)))) #'< phpstan--dumped-types))))))
        (kill-new type)
        (message "Copied %s" type))
    (user-error "No dumped PHPStan types")))

;;;###autoload
(defun phpstan-insert-dumptype (&optional expression prefix-num)
  "Insert PHPStan\\dumpType() expression-statement by EXPRESSION and PREFIX-NUM."
  (interactive
   (list
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (or (thing-at-point 'symbol t) ""))
    current-prefix-arg))
  (let ((template (if prefix-num
                      (cdr phpstan-intert-dump-type-templates)
                    (car phpstan-intert-dump-type-templates))))
    (end-of-line)
    (newline-and-indent)
    (insert template)
    (search-backward "(" (line-beginning-position) t)
    (forward-char)
    (insert expression)))

(provide 'phpstan)
;;; phpstan.el ends here
