;;; bytemancer-theme.el  --- Bytemancer - Theme custom tailored to wrangling bytes -*- lexical-binding: t; -*-

;; TODO: do LICENSE

;; Maintainer: Ryder McMinn
;; Author: Ryder McMinn
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; TODO: do seperate repo

;;; Commentary:

;; TODO

;;; Code:


;;; Theme definition:

(let ((bm-bg0 "")
      (bm-fg "#A0A8CD"))

  (custom-theme-set-faces
   'bytemancer

   ;; Basic
   `(default ((t (:background ,bm-bg0 :foreground ,bm-fg))))
   `(cursor ((t (:background ,bm-fg))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bytemancer)
