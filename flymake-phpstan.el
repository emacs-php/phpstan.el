;;; flymake-phpstan.el --- Flymake backend for PHP using PHPStan  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 31 Mar 2020
;; Version: 0.10.0
;; Keywords: tools, php
;; Homepage: https://github.com/emacs-php/phpstan.el
;; Package-Requires: ((emacs "27.1") (phpstan "0.10.0"))
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

;; Flymake backend for PHP using PHPStan (PHP Static Analysis Tool).
;;
;; Put the following into your .emacs file (~/.emacs.d/init.el)
;;
;;     (add-hook 'php-mode-hook #'flymake-phpstan-turn-on)
;;
;; Like `flycheck-phpstan', this backend reads PHPStan's JSON output, so the
;; identifier and tip of each message are shown, and `phpstan-insert-ignore'
;; and `phpstan-copy-dumped-type' work from a Flymake session too.
;;
;; For Lisp maintainers: see [GNU Flymake manual - 2.2.2 An annotated example backend]
;; https://www.gnu.org/software/emacs/manual/html_node/flymake/An-annotated-example-backend.html

;;; Code:
(require 'cl-lib)
(require 'php-project)
(require 'flymake)
(require 'flymake-proc)
(require 'phpstan)
(eval-when-compile
  (require 'pcase))

(defgroup flymake-phpstan nil
  "Flymake backend for PHP using PHPStan."
  :group 'flymake
  :group 'phpstan)

(defcustom flymake-phpstan-disable-c-mode-hooks t
  "When T, disable `flymake-diagnostic-functions' for `c-mode'."
  :type 'boolean
  :group 'flymake-phpstan)

(defcustom flymake-phpstan-ignore-metadata-list nil
  "Set of metadata items to ignore in PHPStan messages for Flymake."
  :type '(set (const identifier)
              (const tip))
  :group 'flymake-phpstan)

(defcustom flymake-phpstan-metadata-separator "\n"
  "Separator of PHPStan message and metadata."
  :type 'string
  :safe #'stringp
  :group 'flymake-phpstan)

(defconst flymake-phpstan--nofiles-message
  (eval-when-compile (regexp-quote "[ERROR] No files found to analyse.")))

(defvar-local flymake-phpstan--proc nil)

(defun flymake-phpstan--build-message (message)
  "Build the diagnostic text for a PHPStan MESSAGE plist.

Append the identifier and tip, unless disabled by
`flymake-phpstan-ignore-metadata-list', mirroring `flycheck-phpstan'."
  (let* ((msg (plist-get message :message))
         (ignorable (plist-get message :ignorable))
         (identifier (unless (memq 'identifier flymake-phpstan-ignore-metadata-list)
                       (plist-get message :identifier)))
         (tip (unless (memq 'tip flymake-phpstan-ignore-metadata-list)
                (plist-get message :tip)))
         (lines (delq nil
                      (list (when (and identifier ignorable)
                              (concat phpstan-identifier-prefix identifier))
                            (when tip
                              (concat phpstan-tip-message-prefix tip))))))
    (if lines
        (concat msg flymake-phpstan-metadata-separator (string-join lines "\n"))
      msg)))

(defun flymake-phpstan--build-diagnostics (errors source)
  "Build Flymake diagnostics for SOURCE from PHPStan ERRORS.

ERRORS is the alist produced by `phpstan--plist-to-alist' from the JSON
`:files' object.  Every message is attributed to SOURCE by its line, since
editor mode analyzes the one file being edited."
  (cl-loop for (_file . entry) in errors
           append (cl-loop for message in (plist-get entry :messages)
                           for text = (flymake-phpstan--build-message message)
                           for (beg . end) = (flymake-diag-region
                                              source (plist-get message :line))
                           collect (flymake-make-diagnostic source beg end :error text))))

(defun flymake-phpstan--parse (output source)
  "Parse PHPStan OUTPUT and return Flymake diagnostics for SOURCE.

As a side effect, refresh `phpstan--ignorable-errors' and
`phpstan--dumped-types' in SOURCE, so `phpstan-insert-ignore' and
`phpstan-copy-dumped-type' work from Flymake too."
  ;; Look for a line starting with `{', the condition `phpstan--parse-json'
  ;; acts on: it skips anything before that line, so progress a container
  ;; runtime writes to STDERR (merged into STDOUT here) is ignored.
  (if (not (string-match-p "^{" output))
      ;; No report.  A modified buffer with nothing to analyse is expected and
      ;; stays silent; anything else is surfaced as a warning.
      (if (string-match-p flymake-phpstan--nofiles-message output)
          nil
        (list (flymake-make-diagnostic source (point-min) (point-max)
                                       :warning (string-trim output))))
    (with-temp-buffer
      (insert output)
      (let ((errors (phpstan--plist-to-alist
                     (plist-get (phpstan--parse-json (current-buffer)) :files))))
        (with-current-buffer source
          (unless phpstan-disable-buffer-errors
            (phpstan-update-ignorebale-errors-from-json-buffer errors))
          (phpstan-update-dumped-types errors))
        (flymake-phpstan--build-diagnostics errors source)))))

(defun flymake-phpstan-make-process (root command-args report-fn source)
  "Make PHPStan process by ROOT, COMMAND-ARGS, REPORT-FN and SOURCE."
  (let ((default-directory root))
    (make-process
     :name "flymake-phpstan" :noquery t :connection-type 'pipe
     :buffer (generate-new-buffer " *Flymake-PHPStan*")
     :command command-args
     :sentinel
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (unwind-protect
             (when (with-current-buffer source (eq proc flymake-phpstan--proc))
               (funcall report-fn
                        (flymake-phpstan--parse
                         (with-current-buffer (process-buffer proc) (buffer-string))
                         source)))
           (kill-buffer (process-buffer proc))))))))

(defun flymake-phpstan-analyze-original (original)
  "Return non-NIL if ORIGINAL is non-NIL and buffer is not modified."
  (and original (not (buffer-modified-p))))

(defun flymake-phpstan--create-temp-file ()
  "Create temp file and return the path."
  ;; `phpstan-get-command-args' translates the path for the container mount
  ;; point, so return the path as it is on this side.
  (flymake-proc-init-create-temp-buffer-copy 'flymake-proc-create-temp-inplace))

(defun flymake-phpstan (report-fn &rest _ignored-args)
  "Flymake backend for PHPStan report using REPORT-FN."
  (let ((command-args (phpstan-get-command-args :include-executable t)))
    (unless (car command-args)
      (user-error "Cannot find a phpstan executable command"))
    (when (process-live-p flymake-phpstan--proc)
      (kill-process flymake-phpstan--proc))
    (let* ((source (current-buffer))
           (args (phpstan-get-command-args
                  :include-executable t
                  :format "json"
                  :editor (list
                           :analyze-original #'flymake-phpstan-analyze-original
                           :original-file buffer-file-name
                           :temp-file #'flymake-phpstan--create-temp-file
                           :inplace #'flymake-phpstan--create-temp-file))))
      (save-restriction
        (widen)
        (setq flymake-phpstan--proc (flymake-phpstan-make-process (php-project-get-root-dir) args report-fn source))
        (process-send-region flymake-phpstan--proc (point-min) (point-max))
        (process-send-eof flymake-phpstan--proc)))))

;;;###autoload
(defun flymake-phpstan-turn-on ()
  "Enable `flymake-phpstan' as buffer-local Flymake backend."
  (interactive)
  (let ((enabled (phpstan-enabled)))
    (when enabled
      (flymake-mode 1)
      (when flymake-phpstan-disable-c-mode-hooks
        (remove-hook 'flymake-diagnostic-functions #'flymake-cc t))
      (add-hook 'flymake-diagnostic-functions #'flymake-phpstan nil 'local))))

(provide 'flymake-phpstan)
;;; flymake-phpstan.el ends here
