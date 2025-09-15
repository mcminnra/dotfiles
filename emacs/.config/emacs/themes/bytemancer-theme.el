;;; bytemancer-theme.el  --- Bytemancer - Theme custom tailored to mancing bytes -*- lexical-binding: t; -*-

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
(deftheme bytemancer "Theme custom tailored to mancing bytes")

;; Define color palette
(let (
      ;; bg /fg
      (core "#131313")
      (mantle "#191919")
      (crust "#202020")
      (fg "#b0b0b0")
      ;; Colors
      (red "#EE6D85")
      (orange "#F6955B")
      (yellow "#D7A65F")
      (yellow_dim "#ba812e")
      (green "#95C561")
      (green_dim "#587738")
      (blue "#7199EE")
      (blue_dim "#2A3A5A")
      (cyan "#38A89D")
      (purple "#A485DD")
      (purple_dim "#281B27")
      (grey "#4A5057")
    )


;;       local palette = {
;;     black = "#06080A",
;;     bg0 = "#11121D",
;;     bg1 = "#1A1B2A",
;;     bg2 = "#212234",
;;     bg3 = "#353945",
;;     bg4 = "#4A5057",
;;     bg5 = "#282C34",
;;     bg_red = "#FE6D85",
;;     bg_green = "#98C379",
;;     bg_blue = "#9FBBF3",
;;     diff_red = "#773440",
;;     diff_green = "#587738",
;;     diff_blue = "#2A3A5A",
;;     diff_add = "#1E2326",
;;     diff_change = "#262B3D",
;;     diff_delete = "#281B27",
;;     diff_text = "#1C4474",
;;     fg = "#A0A8CD",
;;     red = "#EE6D85",
;;     orange = "#F6955B",
;;     yellow = "#D7A65F",
;;     green = "#95C561",
;;     blue = "#7199EE",
;;     cyan = "#38A89D",
;;     purple = "#A485DD",
;;     grey = "#4A5057",
;;     none = "NONE",
;; }

  (custom-theme-set-faces
   'bytemancer

   ;; Basic faces
   `(default ((t (:background ,mantle :foreground ,fg))))
   `(cursor ((t (:background ,purple))))
   `(region ((t (:background ,blue))))
   `(highlight ((t (:background ,blue))))
   `(hl-line ((t (:background ,blue))))
   `(fringe ((t (:background ,mantle))))
   `(show-paren-match ((t (:background ,purple :foreground ,mantle :weight bold))))
   `(show-paren-mismatch ((t (:background ,red :foreground ,mantle :weight bold))))
   `(window-divider ((t (:background nil :foreground ,mantle))))
   `(vertical-border ((t (:background nil :foreground ,mantle))))

   ;; Font lock faces (syntax highlighting)
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-comment-face ((t (:foreground ,grey :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,orange))))
   `(font-lock-function-name-face ((t (:foreground ,purple :weight bold))))
   `(font-lock-keyword-face ((t (:foreground ,purple :weight bold))))
   `(font-lock-string-face ((t (:foreground ,yellow))))
   `(font-lock-type-face ((t (:foreground ,orange))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-warning-face ((t (:foreground ,red :weight bold))))

   ;; Solaire
   `(solaire-default-face ((t (:background ,core :foreground ,fg))))
   `(solaire-fringe-face ((t (:background ,core :foreground ,fg))))
   `(solaire-line-number-face ((t (:background ,core :foreground ,fg))))
   `(solaire-mode-line-face ((t (:background ,core :foreground ,fg))))
   `(solaire-mode-line-inactive-face ((t (:background ,core :foreground ,fg))))
   `(solaire-header-line-face ((t (:background ,core :foreground ,fg))))

   ;; Mode line
   `(mode-line ((t (:background ,crust :foreground ,fg :box (:line-width 1 :color ,crust)))))
   `(mode-line-inactive ((t (:background ,core :foreground ,grey :box (:line-width 1 :color ,core)))))

   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,purple :weight bold))))

   ;; Line numbers (if using display-line-numbers-mode)
   `(line-number ((t (:background ,mantle :foreground ,grey))))
   `(line-number-current-line ((t (:background ,mantle :foreground ,purple :weight bold))))

   ;; Org mode
   `(org-document-title ((t (:foreground ,orange))))
   `(org-document-info-keyword ((t (:foreground ,grey))))
   `(org-ellipsis ((t (:foreground ,grey))))
   `(org-list-dst ((t (:foreground ,fg))))
   `(org-checkbox-statistics-todo ((t (:foreground ,red))))
   `(org-level-1 ((t (:foreground ,purple :height 1.0))))
   `(org-level-2 ((t (:foreground ,blue :height 1.0))))
   `(org-level-3 ((t (:foreground ,cyan :height 1.0))))
   `(org-level-4 ((t (:foreground ,green :height 1.0))))
   `(org-code ((t (:foreground ,yellow :background ,crust))))
   `(org-block ((t (:background ,crust))))
   `(org-drawer ((t (:foreground ,grey))))
   `(org-special-keyword ((t (:foreground ,grey))))
   `(org-property-value ((t (:foreground ,grey))))
   `(org-date ((t (:foreground ,yellow))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-scheduled-today ((t (:foreground ,fg))))
   `(org-imminent-deadline ((t (:foreground ,red))))
   `(org-upcoming-deadline ((t (:foreground ,yellow))))
   `(org-upcoming-distant-deadline ((t (:foreground ,yellow))))
   `(org-done ((t (:foreground ,grey))))
   `(org-headline-done ((t (:foreground ,grey))))
   `(org-agenda-done ((t (:foreground ,grey))))

  ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bytemancer)
;;; bytemancer-theme.el ends here