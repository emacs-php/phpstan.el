;; -*- mode: eask; lexical-binding: t -*-

(package "phpstan"
         "0.9.0"
         "Interface to PHPStan (PHP static analyzer)")

(website-url "https://github.com/emacs-php/phpstan.el")
(keywords "tools" "php")

(package-file "phpstan.el")
(files "*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "24.3")
(depends-on "compat")
(depends-on "php-mode")
(depends-on "php-runtime")
(depends-on "flycheck")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
