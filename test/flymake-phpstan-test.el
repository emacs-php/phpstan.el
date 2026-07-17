;;; flymake-phpstan-test.el --- Tests for flymake-phpstan.el  -*- lexical-binding: t; -*-

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

;; ERT tests for `flymake-phpstan's JSON parsing: the metadata a message
;; carries, the output shapes a container runtime produces, and the side
;; effect that feeds `phpstan-insert-ignore'.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'flymake-phpstan)

(defconst flymake-phpstan-test--json
  (concat "{\"totals\":{\"errors\":0,\"file_errors\":2},"
          "\"files\":{\"/app/test.php\":{\"errors\":2,\"messages\":["
          "{\"message\":\"Function f not found.\",\"line\":4,\"ignorable\":true,"
          "\"identifier\":\"function.notFound\",\"tip\":\"Learn more.\"},"
          "{\"message\":\"Constant Fooo not found.\",\"line\":7,\"ignorable\":true,"
          "\"identifier\":\"constant.notFound\"}"
          "]}},\"errors\":[]}")
  "A PHPStan JSON report with two errors, one carrying a tip.")

;;; Message building

(ert-deftest flymake-phpstan-test-build-message ()
  (let ((flymake-phpstan-ignore-metadata-list nil)
        (phpstan-identifier-prefix "ID:")
        (phpstan-tip-message-prefix "TIP:")
        (flymake-phpstan-metadata-separator "\n"))
    ;; Message, identifier (ignorable), and tip.
    (should (equal "Function f not found.\nID:function.notFound\nTIP:Learn more."
                   (flymake-phpstan--build-message
                    '(:message "Function f not found." :line 4 :ignorable t
                               :identifier "function.notFound" :tip "Learn more."))))
    ;; A plain message with no metadata is returned unchanged.
    (should (equal "Bare."
                   (flymake-phpstan--build-message '(:message "Bare." :line 1))))))

(ert-deftest flymake-phpstan-test-build-message-ignore-metadata ()
  "`flymake-phpstan-ignore-metadata-list' drops the chosen metadata."
  (let ((phpstan-identifier-prefix "ID:")
        (phpstan-tip-message-prefix "TIP:")
        (flymake-phpstan-metadata-separator "\n"))
    (let ((flymake-phpstan-ignore-metadata-list '(identifier tip)))
      (should (equal "Just the message."
                     (flymake-phpstan--build-message
                      '(:message "Just the message." :line 1 :ignorable t
                                 :identifier "x.y" :tip "hint.")))))))

;;; Parsing

(ert-deftest flymake-phpstan-test-parse-updates-ignorable-errors ()
  "Parsing a report refreshes `phpstan--ignorable-errors' in the source.
This is what lets `phpstan-insert-ignore' work from Flymake."
  (with-temp-buffer
    (let ((source (current-buffer))
          (phpstan-disable-buffer-errors nil))
      (flymake-phpstan--parse flymake-phpstan-test--json source)
      (should (equal '((4 "function.notFound") (7 "constant.notFound"))
                     phpstan--ignorable-errors)))))

(ert-deftest flymake-phpstan-test-parse-skips-non-ignorable ()
  "A non-ignorable message (\"ignorable\":false) is not offered to ignore."
  (with-temp-buffer
    (let ((source (current-buffer))
          (phpstan-disable-buffer-errors nil)
          (json (concat "{\"files\":{\"/x\":{\"messages\":["
                        "{\"message\":\"parse error\",\"line\":1,\"ignorable\":false,"
                        "\"identifier\":\"ignore.parseError\"},"
                        "{\"message\":\"undefined\",\"line\":2,\"ignorable\":true,"
                        "\"identifier\":\"variable.undefined\"}]}}}")))
      (flymake-phpstan--parse json source)
      (should (equal '((2 "variable.undefined")) phpstan--ignorable-errors)))))

(ert-deftest flymake-phpstan-test-parse-json-with-stderr-prefix ()
  "The report is found even when a container prefixes it with progress."
  (with-temp-buffer
    (let* ((source (current-buffer))
           (phpstan-disable-buffer-errors t)
           (output (concat "[0/6] Fetching image\n"
                           "[6/6] Starting container\n"
                           flymake-phpstan-test--json))
           (diags (flymake-phpstan--parse output source)))
      (should (= 2 (length diags)))
      (should (cl-every (lambda (d) (eq :error (flymake-diagnostic-type d))) diags)))))

(ert-deftest flymake-phpstan-test-parse-nofiles-is-silent ()
  "\"No files found to analyse.\" produces no diagnostics."
  (with-temp-buffer
    (let ((source (current-buffer)))
      (should-not (flymake-phpstan--parse
                   " [ERROR] No files found to analyse." source)))))

(ert-deftest flymake-phpstan-test-parse-other-non-json-is-surfaced ()
  "Any other non-JSON output is surfaced as a warning, not dropped.
A container that cannot start, for example, must not look like success."
  (with-temp-buffer
    (let* ((source (current-buffer))
           (diags (flymake-phpstan--parse
                   "failed to connect to the docker API" source)))
      (should (= 1 (length diags)))
      (should (eq :warning (flymake-diagnostic-type (car diags))))
      (should (string-match-p "docker API"
                              (flymake-diagnostic-text (car diags)))))))

(provide 'flymake-phpstan-test)
;;; flymake-phpstan-test.el ends here
