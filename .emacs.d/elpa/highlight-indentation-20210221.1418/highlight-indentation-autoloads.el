;;; highlight-indentation-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation"
;;;;;;  "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation.el

(autoload 'highlight-indentation-mode "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation" "\
Highlight indentation minor mode highlights indentation based on spaces

This is a minor mode.  If called interactively, toggle the
`Highlight-Indentation mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `highlight-indentation-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'highlight-indentation-set-offset "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation" "\
Set indentation offset locally in buffer, will prevent
highlight-indentation from trying to guess indentation offset
from major mode

\(fn OFFSET)" t nil)

(autoload 'highlight-indentation-current-column-mode "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation" "\
Highlight Indentation minor mode displays a vertical bar
corresponding to the indentation of the current line

This is a minor mode.  If called interactively, toggle the
`Highlight-Indentation-Current-Column mode' mode.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `highlight-indentation-current-column-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation" '("highlight-indentation-"))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/highlight-indentation-20210221.1418/highlight-indentation-autoloads.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-indentation-autoloads.el ends here
