;;; phpstan-hover.el --- Hover type display for PHPStan -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 16 Feb 2026
;; Keywords: tools, php
;; Homepage: https://github.com/emacs-php/phpstan.el
;; Package-Requires: ((emacs "26.1") (phpstan "0.9.0"))
;; License: GPL-3.0-or-later

;;; Commentary:

;; Show PHPStan inferred type at point without using Flycheck/Flymake diagnostics.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'php-project)
(require 'phpstan)
(eval-when-compile
  (require 'compat nil t)
  (require 'subr-x)
  (require 'pcase))

(declare-function posframe-hide "ext:posframe" (buffer-or-name))
(declare-function posframe-show "ext:posframe")
(declare-function popup-tip "ext:popup")

(defgroup phpstan-hover nil
  "Hover type display for PHPStan."
  :group 'phpstan)

(defcustom phpstan-hover-idle-delay 0.8
  "Seconds to wait before requesting PHPStan hover data."
  :type 'number)

(defcustom phpstan-hover-display-backend 'auto
  "How hover messages are displayed.

`auto' tries `posframe-show' first (GUI only), then `popup-tip', and finally
`message'."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Posframe" posframe)
                 (const :tag "Popup" popup)
                 (const :tag "Message" message)))

(defcustom phpstan-hover-message-prefix "PHPStan: "
  "Prefix for hover messages."
  :type '(choice
          (string :tag "Custom Label")
          (const :tag "Bookmark Emoji" "🔖 ")
          (const :tag "PHPStan prefix" "PHPStan: ")))

(defcustom phpstan-hover-show-kind-label t
  "When non-nil, show kind labels like return/yield in hover text."
  :type 'boolean)

(defcustom phpstan-hover-debug nil
  "When non-nil, re-signal internal errors to show full backtraces."
  :type 'boolean)

(defcustom phpstan-hover-state-directory
  (expand-file-name "phpstan-hover" temporary-file-directory)
  "Directory for generated helper files and reports."
  :type 'directory)

(defvar-local phpstan-hover--idle-timer nil)
(defvar-local phpstan-hover--process nil)
(defvar-local phpstan-hover--process-buffer nil)
(defvar-local phpstan-hover--cleanup-files nil)
(defvar-local phpstan-hover--report nil)
(defvar-local phpstan-hover--report-tick -1)
(defvar-local phpstan-hover--last-shown nil)
(defvar-local phpstan-hover--last-point nil)
(defvar-local phpstan-hover--last-command-point nil)
(defvar-local phpstan-hover--last-command-window nil)
(defvar-local phpstan-hover--last-display-state nil)
(defvar-local phpstan-hover--config nil)
(defvar-local phpstan-hover--last-command nil)
(defvar phpstan-hover-mode nil)

(defun phpstan-hover--buffer-file ()
  "Return current local buffer file path or nil."
  (when buffer-file-name
    (phpstan--expand-file-name buffer-file-name)))

(defun phpstan-hover--state-dir ()
  "Return state directory for current project."
  (let ((root (or (php-project-get-root-dir) default-directory)))
    (expand-file-name (md5 (expand-file-name root))
                      (file-name-as-directory phpstan-hover-state-directory))))

(defun phpstan-hover--tree-fetcher-template-file ()
  "Return template path of TreeFetcher script."
  (let* ((base-file (or load-file-name
                        (symbol-file 'phpstan-hover-mode 'defun)
                        buffer-file-name))
         (library-dir (and base-file (file-name-directory base-file)))
         (template-file (expand-file-name "php/phpstan-hover-tree-fetcher.php" library-dir)))
    (unless library-dir
      (error "PHPStan hover: cannot resolve library dir (load-file-name=%S buffer-file-name=%S)"
             load-file-name buffer-file-name))
    (unless (file-readable-p template-file)
      (user-error "Template file not found: %s" template-file))
    template-file))

(defun phpstan-hover--write-file (path content)
  "Write CONTENT to PATH in UTF-8."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert content))))

(defsubst phpstan-hover--quote-single (s)
  "Return S with escaped single quotes for PHP/NEON literals."
  (replace-regexp-in-string "'" "\\\\'" s t t))

(defun phpstan-hover--build-config (config-file cache-dir)
  "Build PHPStan config text using CONFIG-FILE and CACHE-DIR."
  (concat
   (if config-file
       (concat "includes:\n"
               (format "  - '%s'\n\n" (phpstan-hover--quote-single config-file)))
     "")
   "\n"
   "rules:\n"
   "  - PHPStanEmacsHoverTreeFetcher\n"
   "\n"
   "parameters:\n"
   (format "  tmpDir: '%s'\n" (phpstan-hover--quote-single cache-dir))
   "\n"
   "services:\n"
   "  -\n"
   "    class: PHPStanEmacsHoverTreeFetcherCollector\n"
   "    tags:\n"
   "      - phpstan.collector\n"))

(defun phpstan-hover--ensure-runtime-files ()
  "Prepare helper files and return runtime plist."
  (let* ((dir (phpstan-hover--state-dir))
         (cache-dir (expand-file-name "cache" dir))
         (report-file (expand-file-name "reported.json" dir))
         (tree-fetcher-file (expand-file-name "TreeFetcher.php" dir))
         (autoload-file (expand-file-name "autoload.php" dir))
         (config-file (expand-file-name "config.neon" dir))
         (user-config (phpstan-get-config-file))
         (user-autoload (phpstan-get-autoload-file))
         (template (with-temp-buffer
                     (insert-file-contents (phpstan-hover--tree-fetcher-template-file))
                     (buffer-string)))
         (tree-fetcher-content
          (replace-regexp-in-string
           "__PHPSTAN_EMACS_HOVER_REPORT_FILE__"
           (replace-regexp-in-string "\\\\" "\\\\\\\\" report-file t t)
           template t t))
         (autoload-content
          (concat
           "<?php\n"
           (if user-autoload
               (format "require_once '%s';\n" (phpstan-hover--quote-single user-autoload))
             "")
           (format "require_once '%s';\n" (phpstan-hover--quote-single tree-fetcher-file))))
         (config-content (phpstan-hover--build-config user-config cache-dir)))
    (make-directory dir t)
    (phpstan-hover--write-file tree-fetcher-file tree-fetcher-content)
    (phpstan-hover--write-file autoload-file autoload-content)
    (phpstan-hover--write-file config-file config-content)
    (setq phpstan-hover--config
          (list :dir dir
                :cache-dir cache-dir
                :report-file report-file
                :tree-fetcher-file tree-fetcher-file
                :autoload-file autoload-file
                :config-file config-file))))

(defun phpstan-hover--create-temp-file ()
  "Create temp file from current buffer and register cleanup."
  (let* ((coding-system-for-write 'utf-8-unix)
         (file (make-temp-file "phpstan-hover-" nil ".php")))
    (write-region (point-min) (point-max) file nil 'silent)
    (prog1 file
      (push file phpstan-hover--cleanup-files))))

(defun phpstan-hover--cleanup-temp-files ()
  "Delete registered temporary files."
  (dolist (file phpstan-hover--cleanup-files)
    (ignore-errors (delete-file file)))
  (setq phpstan-hover--cleanup-files nil))

(defun phpstan-hover--line-byte-column-at-point ()
  "Return 0-based line and byte-column at point."
  (let* ((line (1- (line-number-at-pos nil t)))
         (line-beg (line-beginning-position))
         (point-byte (or (position-bytes (point)) 0))
         (line-beg-byte (or (position-bytes line-beg) 0)))
    (cons line (- point-byte line-beg-byte))))

(defun phpstan-hover--parse-report (report-file)
  "Read REPORT-FILE and return parsed plist JSON data."
  (when (file-readable-p report-file)
    (with-temp-buffer
      (insert-file-contents report-file)
      (phpstan--parse-json (current-buffer)))))

(defsubst phpstan-hover--string-keyword (s)
  "Return keyword symbol for JSON object key string S."
  (intern (concat ":" s)))

(defun phpstan-hover--datum-at-point ()
  "Return hovered datum at point from cached report."
  (let* ((buffer-file (phpstan-hover--buffer-file))
         (normalized-file (when buffer-file
                            (phpstan-normalize-path buffer-file buffer-file)))
         (file-data (and normalized-file
                         (plist-get phpstan-hover--report
                                    (phpstan-hover--string-keyword normalized-file))))
         (line+col (phpstan-hover--line-byte-column-at-point))
         (line (car line+col))
         (col (cdr line+col)))
    (cl-find-if
     (lambda (datum)
       (let* ((pos (plist-get datum :pos))
              (start (plist-get pos :start))
              (end (plist-get pos :end))
              (start-line (plist-get start :line))
              (start-char (plist-get start :char))
              (end-line (plist-get end :line))
              (end-char (plist-get end :char)))
         (and (numberp start-line)
              (numberp start-char)
              (numberp end-line)
              (numberp end-char)
              (or (> line start-line)
                  (and (= line start-line) (>= col start-char)))
              (or (< line end-line)
                  (and (= line end-line) (< col end-char))))))
     file-data)))

;;;###autoload
(defun phpstan-hover-type-at-point (&optional prefer-phpdoc)
  "Return hover type string at point.

If PREFER-PHPDOC is non-nil, return PHPDoc type when available."
  (when-let ((datum (phpstan-hover--datum-at-point)))
    (let ((type (plist-get datum :typeDescr))
          (phpdoc-type (plist-get datum :phpDocType)))
      (if (and prefer-phpdoc
               (stringp phpdoc-type)
               (> (length phpdoc-type) 0))
          phpdoc-type
        type))))

(defun phpstan-hover--format-message (datum)
  "Return display string from DATUM."
  (let* ((type (plist-get datum :typeDescr))
         (phpdoc-type (plist-get datum :phpDocType))
         (name (plist-get datum :name))
         (kind (plist-get datum :kind))
         (body (if (not phpstan-hover-show-kind-label)
                    type
                  (pcase kind
                    ((or "return" (guard (equal name "return")))
                     (format "return: %s" type))
                    ("yield"
                     (format "yield: %s" type))
                    ("yield-from"
                     (format "yield from: %s" type))
                    ((or "const" "class-const")
                     (format "%s: %s" name type))
                    ("call"
                     (format "%s(): %s" name type))
                    (_
                     (format "$%s: %s" name type))))))
    (if (and (stringp phpdoc-type) (> (length phpdoc-type) 0))
        (format "%s%s [PHPDoc: %s]" phpstan-hover-message-prefix body phpdoc-type)
      (concat phpstan-hover-message-prefix body))))

(defun phpstan-hover--resolve-backend ()
  "Resolve display backend.

This honors `phpstan-hover-display-backend'."
  (pcase phpstan-hover-display-backend
    ('auto (cond
            ((and (display-graphic-p) (fboundp 'posframe-show)) 'posframe)
            ((fboundp 'popup-tip) 'popup)
            (t 'message)))
    (_ phpstan-hover-display-backend)))

(defsubst phpstan-hover--posframe-buffer-name ()
  "Return buffer name used by phpstan-hover posframe."
  (format " *phpstan-hover-%s*" (buffer-name)))

(defun phpstan-hover--display-state ()
  "Return current state used to determine if posframe should stay visible."
  (list (current-buffer) (buffer-modified-tick) (point) (selected-window)))

(defun phpstan-hover--check-display-state ()
  "Update display state and return non-nil when unchanged."
  (let* ((current-state (phpstan-hover--display-state)))
    (prog1 (equal phpstan-hover--last-display-state current-state)
      (setq phpstan-hover--last-display-state current-state))))

(defun phpstan-hover--posframe-hidehandler (_info)
  "Hide posframe when point/window/buffer state has changed."
  (not (phpstan-hover--check-display-state)))

(defun phpstan-hover--hide ()
  "Hide existing popup if possible."
  (when (and (fboundp 'posframe-hide) (buffer-live-p (current-buffer)))
    (posframe-hide (phpstan-hover--posframe-buffer-name))))

(defun phpstan-hover--show (text)
  "Show hover TEXT."
  (unless (equal text phpstan-hover--last-shown)
    (setq phpstan-hover--last-shown text)
    (pcase (phpstan-hover--resolve-backend)
      ('posframe
       (phpstan-hover--check-display-state)
       (posframe-show (phpstan-hover--posframe-buffer-name)
                      :string text
                      :position (point)
                      :accept-focus nil
                      :internal-border-width 2
                      :internal-border-color "gray50"
                      :hidehandler #'phpstan-hover--posframe-hidehandler))
      ('popup
       (popup-tip text))
      (_
       (message "%s" text)))))

(defun phpstan-hover--show-at-point ()
  "Show hover text for current point if available."
  (if-let ((datum (phpstan-hover--datum-at-point)))
      (phpstan-hover--show (phpstan-hover--format-message datum))
    (setq phpstan-hover--last-shown nil)
    (phpstan-hover--hide)))

(defun phpstan-hover--process-live-p ()
  "Return non-nil if hover process is alive."
  (and phpstan-hover--process
       (process-live-p phpstan-hover--process)))

(defun phpstan-hover--kill-process ()
  "Stop running hover process and cleanup process buffer."
  (when (phpstan-hover--process-live-p)
    (kill-process phpstan-hover--process))
  (when (buffer-live-p phpstan-hover--process-buffer)
    (kill-buffer phpstan-hover--process-buffer))
  (setq phpstan-hover--process nil)
  (setq phpstan-hover--process-buffer nil)
  (phpstan-hover--cleanup-temp-files))

(defun phpstan-hover--start-analysis ()
  "Start PHPStan process for hover report."
  (when (and phpstan-hover-mode
             (phpstan-enabled)
             (phpstan-hover--buffer-file)
             (not (phpstan-hover--process-live-p)))
    (let* ((runtime (phpstan-hover--ensure-runtime-files))
           (tick (buffer-chars-modified-tick))
           (original-file (phpstan-hover--buffer-file))
           (command (thread-last
                      (phpstan-get-command-args
                       :include-executable t
                       :format "json"
                       :config (plist-get runtime :config-file)
                       :options (list "-a" (plist-get runtime :autoload-file))
                       :editor (list
                                :analyze-original #'phpstan-buffer-not-modified-p
                                :original-file original-file
                                :temp-file #'phpstan-hover--create-temp-file
                                :inplace #'phpstan-hover--create-temp-file))
                      (delq nil))))
      (setq phpstan-hover--process-buffer (generate-new-buffer " *phpstan-hover-process*"))
      (setq phpstan-hover--last-command command)
      (let ((source (current-buffer))
            (report-file (plist-get runtime :report-file)))
        (setq phpstan-hover--process
              (make-process
               :name "phpstan-hover"
               :noquery t
               :command command
               :buffer phpstan-hover--process-buffer
               :sentinel
               (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (when (buffer-live-p source)
                     (with-current-buffer source
                       (setq phpstan-hover--process nil)
                       (setq phpstan-hover--report
                             (or (phpstan-hover--parse-report report-file)
                                 phpstan-hover--report))
                       (setq phpstan-hover--report-tick tick)
                       (phpstan-hover--cleanup-temp-files)
                       (when (equal phpstan-hover--last-point (point))
                         (phpstan-hover--show-at-point))))
                   (when (buffer-live-p (process-buffer proc))
                     (kill-buffer (process-buffer proc)))))))))))

(defun phpstan-hover--idle-run (buffer)
  "Idle callback for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq phpstan-hover--idle-timer nil)
      (when (and phpstan-hover-mode
                 (phpstan-enabled)
                 (phpstan-hover--buffer-file))
        (setq phpstan-hover--last-point (point))
        (phpstan-hover--show-at-point)
        (when (< phpstan-hover--report-tick (buffer-chars-modified-tick))
          (phpstan-hover--start-analysis))))))

(defun phpstan-hover--schedule ()
  "Schedule hover lookup by idle timer."
  (when phpstan-hover-mode
    (let ((point-changed (not (equal phpstan-hover--last-command-point (point))))
          (window-changed (not (eq phpstan-hover--last-command-window (selected-window)))))
      (when (or point-changed window-changed)
        (setq phpstan-hover--last-shown nil)
        (phpstan-hover--hide))
      (setq phpstan-hover--last-command-point (point))
      (setq phpstan-hover--last-command-window (selected-window)))
    (when (timerp phpstan-hover--idle-timer)
      (cancel-timer phpstan-hover--idle-timer))
    (setq phpstan-hover--idle-timer
          (run-with-idle-timer phpstan-hover-idle-delay nil
                               #'phpstan-hover--idle-run
                               (current-buffer)))))

(defun phpstan-hover--teardown ()
  "Cleanup local resources for `phpstan-hover-mode'."
  (when (timerp phpstan-hover--idle-timer)
    (cancel-timer phpstan-hover--idle-timer))
  (setq phpstan-hover--idle-timer nil)
  (setq phpstan-hover--last-display-state nil)
  (setq phpstan-hover--last-command-point nil)
  (setq phpstan-hover--last-command-window nil)
  (phpstan-hover--hide)
  (phpstan-hover--kill-process))

;;;###autoload
(define-minor-mode phpstan-hover-mode
  "Toggle hover type display using PHPStan editor mode reports."
  :init-value nil
  :lighter " PHover"
  :group 'phpstan-hover
  (if phpstan-hover-mode
      (progn
        (add-hook 'post-command-hook #'phpstan-hover--schedule nil t)
        (add-hook 'kill-buffer-hook #'phpstan-hover--teardown nil t)
        (add-hook 'after-save-hook #'phpstan-hover--schedule nil t))
    (remove-hook 'post-command-hook #'phpstan-hover--schedule t)
    (remove-hook 'kill-buffer-hook #'phpstan-hover--teardown t)
    (remove-hook 'after-save-hook #'phpstan-hover--schedule t)
    (phpstan-hover--teardown)))

(provide 'phpstan-hover)
;;; phpstan-hover.el ends here
