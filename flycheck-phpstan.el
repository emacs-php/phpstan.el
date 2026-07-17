;;; flycheck-phpstan.el --- Flycheck integration for PHPStan  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 15 Mar 2018
;; Version: 0.9.0
;; Keywords: tools, php
;; Homepage: https://github.com/emacs-php/phpstan.el
;; Package-Requires: ((emacs "25.1") (flycheck "26") (phpstan "0.9.0"))
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

;; Flycheck integration for PHPStan.
;;
;; Put the following into your .emacs file (~/.emacs.d/init.el)
;;
;;     (defun my-php-mode-setup ()
;;       "My PHP-mode hook."
;;       (require 'flycheck-phpstan)
;;       (flycheck-mode t))
;;
;;     (add-hook 'php-mode-hook 'my-php-mode-setup)
;;
;; ## For Lisp maintainers
;;
;; This is a generic checker (`flycheck-define-generic-checker'), not a command
;; checker (`flycheck-define-checker').  A command checker takes its executable
;; from the car of `:command', which must be a literal string, overridable only
;; through the single string variable `flycheck-CHECKER-executable'.  PHPStan
;; does not fit that shape: `phpstan-executable' may expand to a whole command
;; line such as `docker run --rm -v ...', and it is chosen per project.  So we
;; drive the process ourselves and build the command from `phpstan-executable'.

;;; Code:
(require 'cl-lib)
(require 'flycheck)
(require 'phpstan)

(defvar flycheck-phpstan--temp-buffer-name "*Flycheck PHPStan*")
(defconst flycheck-phpstan--nofiles-message (eval-when-compile (regexp-quote "[ERROR] No files found to analyse.")))

(defcustom flycheck-phpstan-ignore-metadata-list nil
  "Set of metadata items to ignore in PHPStan messages for Flycheck."
  :type '(set (const identifier)
              (const tip))
  :group 'phpstan)

(defcustom flycheck-phpstan-metadata-separator "\n"
  "Separator of PHPStan message and metadata."
  :type 'string
  :safe #'stringp
  :group 'phpstan)

;; Parsing PHPStan output:
(defun flycheck-phpstan-parse-output (output &optional _checker _buffer)
  "Parse PHPStan errors from OUTPUT."
  (let* ((json-buffer (with-current-buffer (flycheck-phpstan--temp-buffer)
                        (erase-buffer)
                        (insert output)
                        (current-buffer)))
         (data (if (string-prefix-p "{" output)
                   (phpstan--parse-json json-buffer)
                 (list (flycheck-error-new-at 1 1 'warning (string-trim output)))))
         (errors (phpstan--plist-to-alist (plist-get data :files))))
    (unless phpstan-disable-buffer-errors
      (phpstan-update-ignorebale-errors-from-json-buffer errors))
    (phpstan-update-dumped-types errors)
    (flycheck-phpstan--build-errors errors)))

(defun flycheck-phpstan--temp-buffer ()
  "Return a temporary buffer for decode JSON."
  (get-buffer-create flycheck-phpstan--temp-buffer-name))

(defun flycheck-phpstan--build-errors (errors)
  "Build Flycheck errors from PHPStan ERRORS."
  (cl-loop for (file . entry) in errors
           append (cl-loop for messages in (plist-get entry :messages)
                           for text = (let* ((msg (plist-get messages :message))
                                             (ignorable (plist-get messages :ignorable))
                                             (identifier (unless (memq 'identifier flycheck-phpstan-ignore-metadata-list)
                                                           (plist-get messages :identifier)))
                                             (tip (unless (memq 'tip flycheck-phpstan-ignore-metadata-list)
                                                    (plist-get messages :tip)))
                                             (lines (list (when (and identifier ignorable)
                                                            (concat phpstan-identifier-prefix identifier))
                                                          (when tip
                                                            (concat phpstan-tip-message-prefix tip))))
                                             (lines (cl-remove-if #'null lines)))
                                        (if (null lines)
                                            msg
                                          (concat msg flycheck-phpstan-metadata-separator
                                                  (string-join lines "\n"))))
                           collect (flycheck-error-new-at (plist-get messages :line)
                                                          nil 'error text
                                                          :filename file))))

(defun flycheck-phpstan-analyze-original (original)
  "Return non-NIL if ORIGINAL is non-NIL and buffer is not modified."
  (and original (not (buffer-modified-p))))

;; Running PHPStan:
(defun flycheck-phpstan--command ()
  "Return the whole PHPStan command line to check the current buffer.

This has the side effect of saving the buffer to a temporary file, which is
registered in `flycheck-temporaries' for later deletion."
  (phpstan-get-command-args
   :include-executable t
   :format "json"
   :editor (list
            :analyze-original #'flycheck-phpstan-analyze-original
            :original-file buffer-file-name
            :temp-file (lambda () (flycheck-save-buffer-to-temp #'flycheck-temp-file-system))
            :inplace (lambda () (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace)))))

(defun flycheck-phpstan--start (checker callback)
  "Start CHECKER, reporting the result to CALLBACK.

Return the process, which Flycheck hands back to `:interrupt'."
  (let (process)
    (condition-case err
        (let ((command (flycheck-phpstan--command))
              ;; `flycheck-phpstan--nofiles-message' matches PHPStan's English
              ;; output, so keep the checker process in the C locale.  Only
              ;; LC_MESSAGES is set, to leave the encoding of the source alone.
              (process-environment (cons "LC_MESSAGES=C" process-environment)))
          (setq process
                (make-process
                 :name (format "flycheck-%s" checker)
                 ;; Do not associate a buffer, to avoid the side effects of
                 ;; attaching a process to the buffer being checked.
                 :buffer nil
                 :command command
                 :noquery t
                 :connection-type 'pipe
                 :filter #'flycheck-phpstan--receive-output
                 :sentinel #'flycheck-phpstan--handle-signal))
          (process-put process 'flycheck-phpstan-checker checker)
          (process-put process 'flycheck-phpstan-callback callback)
          (process-put process 'flycheck-phpstan-buffer (current-buffer))
          ;; Flycheck binds `default-directory' to `:working-directory' around
          ;; this function, so remember it for resolving relative file names.
          (process-put process 'flycheck-phpstan-cwd default-directory)
          ;; Track the temporaries in the process itself, to get rid of the
          ;; buffer-local state as soon as possible.
          (process-put process 'flycheck-phpstan-temporaries flycheck-temporaries)
          (setq flycheck-temporaries nil)
          process)
      (error
       (flycheck-safe-delete-temporaries)
       ;; Deleting the process triggers the sentinel, which deletes the
       ;; temporary files of the process anyway.
       (when process
         (delete-process process))
       (signal (car err) (cdr err))))))

(defun flycheck-phpstan--interrupt (_checker process)
  "Interrupt PROCESS."
  ;; Deleting the process always triggers the sentinel, which does the cleanup.
  (when process
    (delete-process process)))

(defun flycheck-phpstan--receive-output (process output)
  "Accumulate OUTPUT of the PHPStan PROCESS for later parsing."
  (process-put process 'flycheck-phpstan-pending-output
               (cons output (process-get process 'flycheck-phpstan-pending-output))))

(defun flycheck-phpstan--get-output (process)
  "Return the complete output of the PHPStan PROCESS."
  (with-demoted-errors "Error while retrieving process output: %S"
    (apply #'concat (nreverse (process-get process 'flycheck-phpstan-pending-output)))))

(defun flycheck-phpstan--handle-signal (process _event)
  "Handle a signal from the PHPStan PROCESS.

_EVENT is ignored."
  (when (memq (process-status process) '(signal exit))
    (let ((files (process-get process 'flycheck-phpstan-temporaries))
          (buffer (process-get process 'flycheck-phpstan-buffer))
          (callback (process-get process 'flycheck-phpstan-callback))
          (cwd (process-get process 'flycheck-phpstan-cwd)))
      (mapc #'flycheck-safe-delete files)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (condition-case err
              (pcase (process-status process)
                (`signal
                 (funcall callback 'interrupted))
                (`exit
                 (flycheck-phpstan--finish
                  (process-get process 'flycheck-phpstan-checker)
                  (process-exit-status process)
                  files
                  (flycheck-phpstan--get-output process)
                  callback cwd)))
            ((debug error)
             (funcall callback 'errored (error-message-string err)))))))))

(defun flycheck-phpstan--finish (checker exit-status files output callback cwd)
  "Parse OUTPUT of CHECKER and report the result to CALLBACK.

EXIT-STATUS is the exit status of the PHPStan process.  FILES is the list of
temporary files given to PHPStan, used to map reported file names back onto
the buffer.  Relative file names are resolved against CWD.

CALLBACK is always invoked with a status that finishes the syntax check,
because Flycheck gets stuck on the current check otherwise."
  (if (and (buffer-modified-p)
           (string-match-p flycheck-phpstan--nofiles-message output))
      ;; PHPStan found nothing to analyse because the buffer is being edited.
      ;; That is not a result worth showing, but the check must still finish.
      (funcall callback 'finished nil)
    (let ((errors (flycheck-phpstan-parse-output output checker (current-buffer))))
      (when (and (not (equal exit-status 0)) (null errors))
        ;; Warn about a suspicious result, but keep going: `suspicious' does
        ;; not finish the syntax check on its own.
        (funcall callback 'suspicious
                 (format "Flycheck checker %S returned %S, but its output \
contained no errors: %s\nTry installing a more recent version of PHPStan, and \
please open a bug report if the issue persists in the latest release.  Thanks!"
                         checker exit-status output)))
      (funcall callback 'finished
               ;; Fix error file names, by substituting them backwards from the
               ;; temporaries.
               (mapcar (lambda (e) (flycheck-fix-error-filename e files cwd))
                       errors)))))

(defun flycheck-phpstan--verify (_checker)
  "Verify the PHPStan setup of the current buffer."
  (let* ((executable-and-args (ignore-errors (phpstan-get-executable-and-args)))
         (program (car executable-and-args))
         (found (and program
                     (if (file-name-absolute-p program)
                         (and (file-executable-p program) program)
                       (executable-find program))))
         (config-file (phpstan-get-config-file)))
    (list
     (flycheck-verification-result-new
      :label "executable"
      :message (cond (found (format "Found at %s" found))
                     (program (format "%s not found" program))
                     (t "Not found"))
      :face (if found 'success '(bold error)))
     (flycheck-verification-result-new
      :label "command"
      :message (if executable-and-args
                   (mapconcat #'shell-quote-argument executable-and-args " ")
                 "Not available")
      :face (if executable-and-args 'success 'warning))
     (flycheck-verification-result-new
      :label "configuration file"
      :message (if config-file (format "Found at %S" config-file) "Not found")
      :face (if config-file 'success 'warning)))))

(flycheck-define-generic-checker 'phpstan
  "PHP static analyzer based on PHPStan."
  :start #'flycheck-phpstan--start
  :interrupt #'flycheck-phpstan--interrupt
  :verify #'flycheck-phpstan--verify
  ;; `flycheck-define-checker' installs this filter for command checkers, but
  ;; generic checkers default to `identity'.
  :error-filter #'flycheck-sanitize-errors
  :working-directory (lambda (_) (phpstan-get-working-dir))
  :enabled (lambda () (phpstan-enabled))
  :modes '(php-mode php-ts-mode phps-mode))

(add-to-list 'flycheck-checkers 'phpstan t)
(flycheck-add-next-checker 'php 'phpstan)

(provide 'flycheck-phpstan)
;;; flycheck-phpstan.el ends here
