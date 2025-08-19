;;; early-init.el

;; Turn off package soe straight works correctly
(setq package-enable-at-startup nil)

;; When native-compiling, emacs has trouble finding gcc. Give it some help.
(cond
  ((eq 'darwin system-type)
   (progn
    (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/current:/opt/homebrew/opt/libgccjit/lib/gcc/current:/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
    )))
