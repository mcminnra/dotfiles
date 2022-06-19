;;; package -- summary: Ryder's init.el
;;; Commentary:

;;; Code:
;; Set default Directory on Windows
(when (and (eq system-type 'windows-nt) (string= user-real-login-name "rymcminn"))
  (setq default-directory "C:/Users/rymcminn/" ))
(when (and (eq system-type 'windows-nt) (string= user-real-login-name "mcmin"))
  (setq default-directory "C:/Users/mcmin/" ))

;; Set Home on Windows
(when (eq system-type 'windows-nt)
  (setenv "Home" (getenv "UserProfile")))

;; Setup Melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

(require 'use-package)

;; Various Emacs Settings
(scroll-bar-mode -1)                                  ; No Scroll
(tool-bar-mode -1)                                    ; No Toolbar
(menu-bar-mode -1)                                    ; No Menu Bar
(setq inhibit-startup-screen t)                       ; No start screen
(show-paren-mode 1)                                   ; Make Emacs highlight paired parentheses
(setq visible-bell t)                                 ; Make bell visible
(setq backup-directory-alist `(("." . "~/.saves")))   ; Set backupdir
(global-linum-mode t)                                 ; Set line numbers
(toggle-frame-maximized)                              ; Set max window on startup (Mac OSX only?)

;; Font
(set-face-attribute 'default nil :font "Source Code Pro" :height 105)
(set-frame-font "Source Code Pro" nil t)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95))
(add-to-list 'default-frame-alist '(alpha . (95)))

;; Helper Functions
(defun open-terminator ()
  "Opens a Terminator terminal window."
  (interactive)
  (call-process "terminator" nil 0 nil "--working-directory" default-directory))

;; Key Bindings
(global-set-key (kbd "C-`") 'open-terminator)

;; ===============================================
;; Packages
;; ===============================================
;; Indent Guide
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-char ":")

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines) ;; Mark a region, then do this command
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Company
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))

;; Helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; Rainbow Delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; ===============================================
;; Programming Modes
;; ===============================================

;; TODO

;; ===============================================
;; Org Stuff
;; ===============================================
;; Org
(require 'org)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-agenda-files (list "~/org/northstar.org"
			     "~/org/tasks.org"
			     "~/org/work.org"
			     "~/org/ph.org"
			     "~/org/e.org"
			     "~/org/l.org"))
(setq org-deadline-warning-days 90)
(setq org-agenda-deadline-faces
      '((0.92 . org-warning)
        (0.84 . org-upcoming-deadline)
        (0.0 . default)))
(setq org-habit-show-all-today t)
(setq org-habit-following-days 1)
(setq org-habit-preceding-days 29)
(setq org-habit-graph-column 50)
(setq   org-enable-priority-commands t
    org-highest-priority ?A
    org-default-priority ?E
    org-lowest-priority ?E
)
(setq org-agenda-sorting-strategy
      '((agenda habit-down deadline-up scheduled-up time-up priority-down category-keep)
        (todo   priority-down category-keep todo-state-up)
        (tags   priority-down category-keep)
        (search category-keep)))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "STARTED(s)" "REVIEW(r)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")
	(sequence "REOCCURING(e)" "|" "DONE(f)")
	(sequence "HOBBY(H)" "PROJECT(P)" "EXPERIENCE(E)" "|" "COMPLETED(C)" "ARCHIVED(A)")
	(sequence "BACKLOG(B)" "IN-NOTEBOOK(O)" "IN-NPML(N)" "IN-REPO(R)" "WITH-NOTES(W)" "|" "ARCHIVED(A)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#30acec" :weight bold))
	("WAITING" . (:foreground "#339989" :weight bold))
	("STARTED" . (:foreground "#725ac1" :weight bold))
	("REVIEW" . (:foreground "#f7b801" :weight bold))
	("BLOCKED" . (:foreground "#f6511d" :weight bold))
	("DONE" . (:foreground "#6a994e" :weight bold))
	("CANCELLED" . (:foreground "#d64a3b" :weight bold))
	;; Reoccuring
	("REOCCURING" . (:foreground "#339989" :weight bold))
	;; Project, Hobby, Learning Flow
	("HOBBY" . (:foreground "white" :background "darkgreen" :weight bold))
	("PROJECT" . (:foreground "white" :background "purple" :weight bold))
	("EXPERIENCE" . (:foreground "white" :background "orange" :weight bold))
	("COMPLETED" . (:foreground "#80c34f" :weight bold))
	("ARCHIVED" . (:foreground "#d64a3b" :weight bold))
	;; Learning Flow
	("BACKLOG" . (:foreground "#30acec" :weight bold))
	("IN-NOTEBOOK" . (:foreground "white" :background "#30acec" :weight bold))
	("IN-NPML" . (:foreground "white" :background "purple" :weight bold))
	("IN-REPO" . (:foreground "white" :background "#7cb518" :weight bold))
	("WITH-NOTES" . (:foreground "white" :background "#a47e1b" :weight bold))))
(setq org-priority-faces '((?A . (:foreground "green"))
                           (?B . (:foreground "DeepSkyBlue"))
                           (?C . (:foreground "yellow"))
			                     (?D . (:foreground "orange red"))
                           (?E . (:foreground "red"))))

(defun org-cycle-agenda-files ()
  "Cycle through the files in `org-agenda-files'.
If the current buffer visits an agenda file, find the next one in the list.
If the current buffer does not, find the first agenda file."
  (interactive)
  (let* ((fs (org-agenda-files t))
   (files (append fs (list (car fs))))
   (tcf (if buffer-file-name (file-truename buffer-file-name)))
   file)
    (unless files (user-error "No agenda files"))
    (catch 'exit
      (while (setq file (pop files))
  (if (equal (file-truename file) tcf)
      (when (car files)
        (find-file (car files))
        (throw 'exit t))))
      (find-file (car fs)))
    (if (buffer-base-buffer) (org-pop-to-buffer-same-window (buffer-base-buffer)))))


;;;; Emacs Auto-Generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(ansi-term-color-vector
   [unspecified "#151515" "#fb9fb1" "#acc267" "#ddb26f" "#6fc2ef" "#e1a3ee" "#6fc2ef" "#d0d0d0"])
 '(beacon-color "#f2777a")
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "9aac6c5b42a96e1bc30370fce81fd1032dabe1791f264457d48798867d4ef173" "1a17e87255654f75c4ee9f51ff7c5ef9a2d7b825bbc738920d9c1ec6f880c8bc" "d0f7d834242581e63a93d0231668c3571d5135debf79baa04ca8f9f5a323ed36" "a942a65c24e9e0191d84a603893df8d5b39d09cadee42fbed381809739cda1f9" "8e660fd9166774764d69a4c19a86822f656abaacf6584531b2a01949452e441f" "8f4fb38b202ca52d25bdf842dff3143d74dc3ddef8b4b3a9094e0ce7b84dd2f2" "904696ae3aa68386db5b9d1cc7d70bc2ea2e469da9870548c74875cbbf4ef55e" "0a23bec6af4f1b82c3e71c09f5287290b91c00fe1acee83463644e0916d2e4d8" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "065efdd71e6d1502877fd5621b984cded01717930639ded0e569e1724d058af8" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "85d609b07346d3220e7da1e0b87f66d11b2eeddad945cac775e80d2c1adb0066" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "31e64af34ba56d5a3e85e4bebefe2fb8d9d431d4244c6e6d95369a643786a40e" "4b207752aa69c0b182c6c3b8e810bbf3afa429ff06f274c8ca52f8df7623eb60" "60668f4b17b8b8780d50976c0788abed190353d21d3371b8f244dd44c103b0ea" "10e3d04d524c42b71496e6c2e770c8e18b153fcfcc838947094dad8e5aa02cef" "759416a7a5f5cb6b8cb26e6db2cf70026aa2324083a888015ee2cad0320f7f19" "d2c61aa11872e2977a07969f92630a49e30975220a079cd39bec361b773b4eb3" "4a7abcca7cfa2ccdf4d7804f1162dd0353ce766b1277e8ee2ac7ee27bfbb408f" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "2a739405edf418b8581dcd176aaf695d319f99e3488224a3c495cb0f9fd814e3" default))
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(hl-sexp-background-color "#1c1f26")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(package-selected-packages
   '(markdown-mode ein gruvbox-theme base16-theme rainbow-delimiters flycheck elpy multiple-cursors transpose-frame material-theme xresources-theme 0blayout theme-changer company-anaconda anaconda-mode neotree helm-flyspell org-gnome latex-preview-pane yaml-mode doom-themes color-theme-sanityinc-tomorrow dracula-theme))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
