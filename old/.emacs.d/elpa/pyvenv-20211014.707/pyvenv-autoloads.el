;;; pyvenv-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv"
;;;;;;  "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv.el

(autoload 'pyvenv-activate "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" "\
Activate the virtual environment in DIRECTORY.

\(fn DIRECTORY)" t nil)

(autoload 'pyvenv-deactivate "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" "\
Deactivate any current virtual environment." t nil)

(autoload 'pyvenv-workon "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" "\
Activate a virtual environment from $WORKON_HOME.

If the virtual environment NAME is already active, this function
does not try to reactivate the environment.

\(fn NAME)" t nil)

(defvar pyvenv-mode nil "\
Non-nil if Pyvenv mode is enabled.
See the `pyvenv-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-mode'.")

(custom-autoload 'pyvenv-mode "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" nil)

(autoload 'pyvenv-mode "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" "\
Global minor mode for pyvenv.

This is a minor mode.  If called interactively, toggle the
`Pyvenv mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pyvenv-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

Will show the current virtualenv in the mode line, and respect a
`pyvenv-workon' setting in files.

\(fn &optional ARG)" t nil)

(defvar pyvenv-tracking-mode nil "\
Non-nil if Pyvenv-Tracking mode is enabled.
See the `pyvenv-tracking-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-tracking-mode'.")

(custom-autoload 'pyvenv-tracking-mode "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" nil)

(autoload 'pyvenv-tracking-mode "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" "\
Global minor mode to track the current virtualenv.

This is a minor mode.  If called interactively, toggle the
`Pyvenv-Tracking mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pyvenv-tracking-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When this mode is active, pyvenv will activate a buffer-specific
virtualenv whenever the user switches to a buffer with a
buffer-local `pyvenv-workon' or `pyvenv-activate' variable.

\(fn &optional ARG)" t nil)

(autoload 'pyvenv-restart-python "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" "\
Restart Python inferior processes." t nil)

(register-definition-prefixes "../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv" '("pyvenv-"))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/pyvenv-20211014.707/pyvenv-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyvenv-autoloads.el ends here
