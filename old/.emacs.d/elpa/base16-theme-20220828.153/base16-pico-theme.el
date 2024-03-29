;; base16-pico-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/base16-project/base16)

;;; Authors:
;; Scheme: PICO-8 (http://www.lexaloffle.com/pico-8.php)
;; Template: Kaleb Elwert <belak@coded.io>

;;; Code:

(require 'base16-theme)

(defvar base16-pico-theme-colors
  '(:base00 "#000000"
    :base01 "#1d2b53"
    :base02 "#7e2553"
    :base03 "#008751"
    :base04 "#ab5236"
    :base05 "#5f574f"
    :base06 "#c2c3c7"
    :base07 "#fff1e8"
    :base08 "#ff004d"
    :base09 "#ffa300"
    :base0A "#fff024"
    :base0B "#00e756"
    :base0C "#29adff"
    :base0D "#83769c"
    :base0E "#ff77a8"
    :base0F "#ffccaa")
  "All colors for Base16 Pico are defined here.")

;; Define the theme
(deftheme base16-pico)

;; Add all the faces to the theme
(base16-theme-define 'base16-pico base16-pico-theme-colors)

;; Mark the theme as provided
(provide-theme 'base16-pico)

(provide 'base16-pico-theme)

;;; base16-pico-theme.el ends here
