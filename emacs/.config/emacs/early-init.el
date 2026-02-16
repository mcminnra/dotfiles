;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Code:

;; Increase GC threshold during startup for faster load, restore after init
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024)))) ; 16MB

;; Disable package.el (elpaca manages packages)
(setq package-enable-at-startup nil)

;; macOS: native-comp needs help finding gcc libraries
(when (eq system-type 'darwin)
  (setenv "LIBRARY_PATH"
          (string-join
           '("/opt/homebrew/opt/gcc/lib/gcc/current"
             "/opt/homebrew/opt/libgccjit/lib/gcc/current"
             "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
           ":"))
  (setq native-comp-async-report-warnings-errors nil))

;;; early-init.el ends here
