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
      (crust "#1A1B2A")
      (surface "#2b2d45")
      (fg "#b0b0b0")
      ;; Colors
      (red "#EE6D85")
      (red_bg "#773440")
      (orange "#F6955B")
      (yellow "#D7A65F")
      (yellow_dim "#ba812e")
      (green "#95C561")
      (green_bg "#587738")
      (green_dim "#587738")
      (blue "#7199EE")
      (blue_bg "#2A3A5A")
      (cyan "#38A89D")
      (purple "#A485DD")
      (purple_dim "#281B27")
      (grey "#4A5057")
      (white "#e0e2ea")
    )


;;       local palette = {
;;     black = "",
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
   `(region ((t (:background ,surface))))
   `(highlight ((t (:background ,surface))))
   `(hl-line ((t (:background ,crust))))
   `(fringe ((t (:background ,mantle))))
   `(match ((t (:background ,surface))))
   `(link ((t (:foreground ,cyan :underline ,cyan))))
   `(show-paren-match ((t (:background ,purple :foreground ,mantle :weight bold))))
   `(show-paren-mismatch ((t (:background ,red :foreground ,mantle :weight bold))))
   `(window-divider ((t (:background nil :foreground ,mantle))))
   `(vertical-border ((t (:background nil :foreground ,mantle))))
   `(error ((t (:foreground ,red :weight bold))))
   `(warning ((t (:foreground ,orange :weight bold))))
   `(success ((t (:foreground ,green :weight bold))))

   ;; Font lock faces (syntax highlighting)
   `(font-lock-builtin-face ((t (:foreground ,purple))))
   `(font-lock-comment-face ((t (:foreground ,grey :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,orange))))
   `(font-lock-number-face ((t (:foreground ,purple))))
   `(font-lock-function-name-face ((t (:foreground ,green))))
   `(font-lock-keyword-face ((t (:foreground ,red))))
   `(font-lock-operator-face ((t (:foreground ,red))))
   `(font-lock-string-face ((t (:foreground ,yellow))))
   `(font-lock-doc-face ((t (:foreground ,yellow))))
   `(font-lock-type-face ((t (:foreground ,blue :slant italic))))
   `(font-lock-variable-name-face ((t (:foreground ,white))))
   `(font-lock-variable-use-face ((t (:foreground ,white))))
   `(font-lock-property-use-face ((t (:foreground ,white))))
   `(font-lock-warning-face ((t (:foreground ,red :weight bold))))

   ;; Helm
   `(helm-source-header ((t (:background ,crust :foreground ,fg))))
   `(helm-selection ((t (:background ,surface))))
   `(helm-ff-directory ((t (:foreground ,red))))
   `(mode-line ((t (:background ,crust :foreground ,orange :box (:line-width 1 :color ,orange)))))

   ;; Solaire
   `(solaire-default-face ((t (:background ,core :foreground ,fg))))
   `(solaire-fringe-face ((t (:background ,core :foreground ,fg))))
   `(solaire-line-number-face ((t (:background ,core :foreground ,fg))))
   `(solaire-mode-line-face ((t (:background ,core :foreground ,fg))))
   `(solaire-mode-line-inactive-face ((t (:background ,core :foreground ,fg))))
   `(solaire-header-line-face ((t (:background ,core :foreground ,fg))))

   ;; Mode line
   `(mode-line ((t (:background ,mantle :foreground ,fg :box (:line-width 1 :color ,crust)))))
   `(mode-line-inactive ((t (:background ,core :foreground ,fg))))
   `(mode-line-active ((t (:background ,crust :foreground ,fg))))
   `(mode-line-highlight ((t (:background ,purple :foreground ,fg))))

   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,purple :weight bold))))

   ;; Line numbers
   `(line-number ((t (:background ,mantle :foreground ,grey))))
   `(line-number-current-line ((t (:background ,mantle :foreground ,purple :weight bold))))

   ;; Rainbow Delimiters
   `(rainbow-delimiters-base-error-face ((t (:foreground ,red))))
   `(rainbow-delimiters-base-face ((t (:foreground ,fg)))) 
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,purple))))        
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,cyan))))        
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))        
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,purple))))        
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,cyan))))        
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,blue))))       
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,purple))))        
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,cyan))))       
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,blue))))         
   `(rainbow-delimiters-mismatched-face ((t (:weight bold))))     
   `(rainbow-delimiters-unmatched-face ((t (:weight bold))))       

   ;; Org mode
   `(org-document-title ((t (:foreground ,orange))))
   `(org-document-info-keyword ((t (:foreground ,grey :slant italic))))
   `(org-ellipsis ((t (:foreground ,grey))))
   `(org-list-dst ((t (:foreground ,fg))))
   `(org-checkbox-statistics-todo ((t (:foreground ,red))))
   `(org-level-1 ((t (:foreground ,purple :height 1.0))))
   `(org-level-2 ((t (:foreground ,blue :height 1.0))))
   `(org-level-3 ((t (:foreground ,green :height 1.0))))
   `(org-level-4 ((t (:foreground ,purple :height 1.0))))
   `(org-level-5 ((t (:foreground ,blue :height 1.0))))
   `(org-level-6 ((t (:foreground ,green :height 1.0))))
   `(org-level-7 ((t (:foreground ,purple :height 1.0))))
   `(org-level-8 ((t (:foreground ,blue :height 1.0))))
   `(org-level-9 ((t (:foreground ,green :height 1.0))))
   `(org-tag ((t (:foreground ,grey :slant italic))))
   `(org-verbatim ((t (:foreground ,red :slant italic))))
   `(org-code ((t (:foreground ,yellow :background ,crust))))
   `(org-block ((t (:background ,crust))))
   `(org-drawer ((t (:foreground ,grey))))
   `(org-special-keyword ((t (:foreground ,grey))))
   `(org-property-value ((t (:foreground ,grey))))
   `(org-done ((t (:foreground ,grey))))
   `(org-headline-done ((t (:foreground ,grey))))
   `(org-table ((t (:foreground ,blue))))
   `(org-table-row ((t (:foreground ,cyan))))
   ; Date-related
   `(org-date ((t (:foreground ,yellow))))
   `(org-time-grid ((t (:foreground ,yellow))))
   `(org-scheduled ((t (:foreground ,fg))))
   `(org-scheduled-today ((t (:foreground ,fg))))
   `(org-scheduled-previously ((t (:foreground ,orange))))
   `(org-imminent-deadline ((t (:foreground ,red))))
   `(org-upcoming-deadline ((t (:foreground ,yellow))))
   `(org-upcoming-distant-deadline ((t (:foreground ,yellow))))
   ; Agenda
   `(org-agenda-done ((t (:foreground ,grey))))
   `(org-agenda-structure ((t (:foreground ,blue))))
   ; Habit
   `(org-habit-ready-face ((t (:background ,green_bg :foreground ,fg))))
   `(org-habit-overdue-face ((t (:background ,red_bg :foreground ,fg))))
   `(org-habit-clear-future-face ((t (:background ,mantle :foreground ,fg))))
   `(org-habit-clear-face ((t (:background ,green_bg :foreground ,fg))))
   `(org-habit-alert-future-face ((t (:background ,red_bg :foreground ,fg))))
   `(org-habit-alert-face ((t (:background ,blue_bg :foreground ,fg))))


  ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'bytemancer)
;;; bytemancer-theme.el ends here