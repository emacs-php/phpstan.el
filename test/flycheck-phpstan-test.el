;;; flycheck-phpstan-test.el --- Tests for flycheck-phpstan.el  -*- lexical-binding: t; -*-

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

;; ERT tests for `flycheck-phpstan-parse-output', covering the output shapes a
;; container runtime produces (JSON preceded by progress on STDERR) and the
;; no-JSON fallback.

;;; Code:
(require 'ert)
(require 'flycheck-phpstan)

(defconst flycheck-phpstan-test--json
  (concat "{\"totals\":{\"errors\":0,\"file_errors\":1},"
          "\"files\":{\"/app/test.php\":{\"errors\":1,\"messages\":["
          "{\"message\":\"Function f not found.\",\"line\":4,\"ignorable\":true}"
          "]}},\"errors\":[]}")
  "A minimal PHPStan JSON report with one error on line 4.")

(ert-deftest flycheck-phpstan-test-parse-json ()
  "A plain JSON report yields the error it describes."
  (let* ((phpstan-disable-buffer-errors t)
         (errors (flycheck-phpstan-parse-output flycheck-phpstan-test--json)))
    (should (= 1 (length errors)))
    (should (= 4 (flycheck-error-line (car errors))))
    (should (string-match-p "Function f not found"
                            (flycheck-error-message (car errors))))))

(ert-deftest flycheck-phpstan-test-parse-json-with-stderr-prefix ()
  "The report must be found even when a runtime prefixes it with progress.
Apple container writes progress to STDERR, which the checker merges into
STDOUT, so the JSON does not start at the beginning of the output."
  (let* ((phpstan-disable-buffer-errors t)
         (output (concat "[0/6] Fetching image\n"
                         "[6/6] Starting container\n"
                         flycheck-phpstan-test--json))
         (errors (flycheck-phpstan-parse-output output)))
    (should (= 1 (length errors)))
    (should (= 4 (flycheck-error-line (car errors))))))

(ert-deftest flycheck-phpstan-test-parse-non-json-is-surfaced ()
  "When there is no JSON report, the raw output is surfaced, not discarded.
A swallowed fallback would show a failing PHPStan as a clean buffer."
  (let* ((phpstan-disable-buffer-errors t)
         (output "Bootstrap file /app/tests/bootstrap.php does not exist.")
         (errors (flycheck-phpstan-parse-output output)))
    (should (= 1 (length errors)))
    (should (eq 'warning (flycheck-error-level (car errors))))
    (should (string-match-p "Bootstrap file"
                            (flycheck-error-message (car errors))))))

(provide 'flycheck-phpstan-test)
;;; flycheck-phpstan-test.el ends here
