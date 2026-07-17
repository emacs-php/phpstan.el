;;; phpstan-test.el --- Tests for phpstan.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Friends of Emacs-PHP development

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

;; ERT tests for the parts of phpstan.el that can be exercised without a
;; running PHPStan.  Anything that would touch the project or the filesystem
;; is stubbed, so the tests stay hermetic.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'phpstan)

;;; Utilities

(ert-deftest phpstan-test-plist-to-alist ()
  (should (equal '(("a" . 1) ("b" . 2))
                 (phpstan--plist-to-alist '(:a 1 :b 2))))
  (should (equal nil (phpstan--plist-to-alist nil))))

;;; JSON parsing

(ert-deftest phpstan-test-parse-json-false-is-nil ()
  "JSON false and null read as nil, not a truthy symbol.
`ignorable' is read in a truthy context, so `false' must be nil."
  (with-temp-buffer
    (insert "{\"a\":false,\"b\":null,\"c\":true}")
    (let ((data (phpstan--parse-json (current-buffer))))
      (should-not (plist-get data :a))
      (should-not (plist-get data :b))
      (should (eq t (plist-get data :c))))))

(ert-deftest phpstan-test-update-ignorable-skips-non-ignorable ()
  "Only ignorable messages feed `phpstan--ignorable-errors'.
A non-ignorable message (\"ignorable\":false) must not be offered to
`phpstan-insert-ignore'."
  (with-temp-buffer
    (insert (concat "{\"files\":{\"/x\":{\"messages\":["
                    "{\"message\":\"parse error\",\"line\":1,\"ignorable\":false,"
                    "\"identifier\":\"ignore.parseError\"},"
                    "{\"message\":\"undefined\",\"line\":2,\"ignorable\":true,"
                    "\"identifier\":\"variable.undefined\"}]}}}"))
    (let ((errors (phpstan--plist-to-alist
                   (plist-get (phpstan--parse-json (current-buffer)) :files))))
      (phpstan-update-ignorebale-errors-from-json-buffer errors)
      (should (equal '((2 "variable.undefined")) phpstan--ignorable-errors)))))

;;; Container runtime detection

(ert-deftest phpstan-test-container-runtime-command ()
  "Only the symbol forms ask phpstan.el to build a `run' command line."
  (let ((phpstan-executable 'docker))
    (should (equal "docker" (phpstan--container-runtime-command))))
  (let ((phpstan-executable 'container))
    (should (equal "container" (phpstan--container-runtime-command))))
  ;; A complete command line must not be rewritten, so this returns nil.
  (let ((phpstan-executable '("docker" "run" "--rm" "img")))
    (should-not (phpstan--container-runtime-command)))
  (let ((phpstan-executable "/usr/bin/phpstan"))
    (should-not (phpstan--container-runtime-command)))
  (let ((phpstan-executable nil))
    (should-not (phpstan--container-runtime-command))))

(ert-deftest phpstan-test-container-executable-p ()
  "Recognize a container even in the explicit command-line form."
  (dolist (exe '(docker container))
    (let ((phpstan-executable exe))
      (should (phpstan--container-executable-p))))
  (let ((phpstan-executable '("docker" "run" "--rm" "img")))
    (should (phpstan--container-executable-p)))
  (let ((phpstan-executable '("container" "run" "--rm" "img")))
    (should (phpstan--container-executable-p)))
  ;; An unknown runtime is not treated as a container.
  (let ((phpstan-executable '("podman" "run" "img")))
    (should-not (phpstan--container-executable-p)))
  (let ((phpstan-executable "/usr/bin/phpstan"))
    (should-not (phpstan--container-executable-p)))
  (let ((phpstan-executable nil))
    (should-not (phpstan--container-executable-p))))

;;; Version parsing

(ert-deftest phpstan-test-version-from-output ()
  (should (equal "1.12.33"
                 (phpstan--version-from-output
                  "PHPStan - PHP Static Analysis Tool 1.12.33\n")))
  ;; A container runtime prints progress before the version; the last line wins.
  (should (equal "2.2.5"
                 (phpstan--version-from-output
                  "[0/6] Fetching image\n[6/6] Starting container\nPHPStan - PHP Static Analysis Tool 2.2.5\n")))
  (should-not (phpstan--version-from-output nil))
  (should-not (phpstan--version-from-output "")))

(ert-deftest phpstan-test-editor-mode-available-p ()
  "Version gating, exercised through the cache to avoid shelling out."
  (let ((phpstan-activate-editor-mode nil))
    ;; The alist is keyed by the whole command line joined with spaces.
    (cl-flet ((available (version)
                (let ((phpstan-executable-versions-alist (list (cons "phpstan" version))))
                  (phpstan-editor-mode-available-p "phpstan"))))
      (should (available "1.12.27"))
      (should (available "1.13.0"))
      (should (available "2.1.17"))
      (should (available "2.2.5"))
      (should (available "1.12.99-dev@abcdef"))
      (should-not (available "1.12.26"))
      (should-not (available "2.1.16"))
      (should-not (available ""))))
  ;; Explicit overrides ignore the version entirely.
  (let ((phpstan-activate-editor-mode 'enabled))
    (should (phpstan-editor-mode-available-p "anything")))
  (let ((phpstan-activate-editor-mode 'disabled))
    (should-not (phpstan-editor-mode-available-p "anything"))))

;;; Path normalization

(ert-deftest phpstan-test-normalize-path ()
  ;; Use a real absolute root and derive the expected value with the same
  ;; `expand-file-name' the code uses, so the test survives Windows drive
  ;; letters and separators rather than assuming a Unix-shaped "/proj/".
  (let ((root (file-name-as-directory
               (expand-file-name "phpstan-test-proj" temporary-file-directory))))
    (cl-letf (((symbol-function 'php-project-get-root-dir) (lambda () root)))
      (let ((src (expand-file-name "src/A.php" root)))
        ;; A containerized PHPStan sees the project under its mount point.
        (let ((phpstan-executable 'docker)
              (phpstan-replace-path-prefix nil))
          (should (equal (expand-file-name "src/A.php" "/app")
                         (phpstan-normalize-path src))))
        ;; A local executable leaves the path alone.
        (let ((phpstan-executable nil)
              (phpstan-replace-path-prefix nil))
          (should (equal src (phpstan-normalize-path src))))))))

(ert-deftest phpstan-test-normalize-path-nil ()
  "A nil path yields nil rather than erroring, even under a container.
`phpstan-get-config-file' returns nil when a project has no configuration,
and a containerized run would otherwise pass that nil to
`replace-regexp-in-string'."
  (cl-letf (((symbol-function 'php-project-get-root-dir) (lambda () "/proj/")))
    (let ((phpstan-replace-path-prefix nil))
      (dolist (exe '(docker container ("docker" "run" "img") nil "/bin/phpstan"))
        (let ((phpstan-executable exe))
          (should-not (phpstan-normalize-path nil))))
      ;; The optional SOURCE fallback still applies when it is given.
      (let ((phpstan-executable 'docker))
        (should (equal "fallback" (phpstan-normalize-path nil "fallback")))))))

;;; Command line construction

(defmacro phpstan-test--with-stubbed-project (&rest body)
  "Run BODY with the project root and config file stubbed to fixed values.
The root is a real absolute path under `temporary-file-directory', so paths
survive `expand-file-name' on every platform."
  (declare (indent 0))
  `(let ((phpstan-test--root (file-name-as-directory
                              (expand-file-name "phpstan-test-proj"
                                                temporary-file-directory))))
     (cl-letf (((symbol-function 'php-project-get-root-dir)
                (lambda () phpstan-test--root))
               ((symbol-function 'phpstan-get-config-file)
                (lambda () (expand-file-name "phpstan.neon" phpstan-test--root))))
       (let ((phpstan-replace-path-prefix nil)
             (phpstan-autoload-file nil)
             (phpstan-memory-limit nil)
             (phpstan-level nil)
             (phpstan-use-xdebug-option nil)
             (phpstan--use-xdebug-option nil))
         ,@body))))

(ert-deftest phpstan-test-command-args-keeps-command-name ()
  "The `(STRING . (ARGUMENTS ...))' form must run the command, not its first arg."
  (phpstan-test--with-stubbed-project
    (let ((phpstan-executable '("docker" "run" "--rm" "-v" "/proj:/app" "img")))
      (let ((args (phpstan-get-command-args :include-executable t)))
        (should (equal "docker" (car args)))
        (should (equal '("docker" "run" "--rm" "-v" "/proj:/app" "img" "analyze")
                       (seq-take args 7)))))))

(ert-deftest phpstan-test-command-args-does-not-mutate-executable ()
  "`phpstan-get-command-args' must not grow the caller's `phpstan-executable'."
  (phpstan-test--with-stubbed-project
    (let* ((original (list "docker" "run" "--rm" "img"))
           (phpstan-executable original))
      (phpstan-get-command-args :include-executable t)
      (phpstan-get-command-args :include-executable t)
      (should (equal '("docker" "run" "--rm" "img") original))
      ;; Two calls must produce the same thing.
      (should (equal (phpstan-get-command-args :include-executable t)
                     (phpstan-get-command-args :include-executable t))))))

(ert-deftest phpstan-test-command-args-does-not-mutate-options ()
  "The `:options' list is the caller's; it must come back unchanged."
  (phpstan-test--with-stubbed-project
    (let ((phpstan-executable "/bin/phpstan")
          (options (list "--generate-baseline")))
      (cl-letf (((symbol-function 'phpstan-get-executable-and-args)
                 (lambda () (list "/bin/phpstan"))))
        (phpstan-get-command-args :include-executable t :options options)
        (phpstan-get-command-args :include-executable t :options options)
        (should (equal '("--generate-baseline") options))))))

(ert-deftest phpstan-test-command-args-normalizes-config-for-container ()
  "The config file is rewritten to the container mount point."
  (phpstan-test--with-stubbed-project
    (let ((phpstan-executable 'docker))
      (let ((args (phpstan-get-command-args :include-executable t)))
        (should (member (expand-file-name "phpstan.neon" "/app") args))
        (should-not (member (phpstan-get-config-file) args))))))

(ert-deftest phpstan-test-command-args-without-config-under-container ()
  "A container run with no configuration file must not error.
`phpstan-get-config-file' returns nil then, and normalizing it used to
error; the command line should simply omit the `-c' flag."
  (phpstan-test--with-stubbed-project
    (cl-letf (((symbol-function 'phpstan-get-config-file) (lambda () nil)))
      (let ((phpstan-executable 'docker))
        (let ((args (phpstan-get-command-args :include-executable t)))
          (should-not (member "-c" args))
          (should (member "analyze" args)))))))

(provide 'phpstan-test)
;;; phpstan-test.el ends here
