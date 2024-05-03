;; init.el

;; Package Archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Setup straight.el package manage
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)     ; turn off emacs default package.el
(setq straight-use-package-by-default t) ; Have use-package also invoke straight.el

;; Font
; Set font size depending on system (Windows weird I guess)
(cond 
  ((eq `windows-nt system-type)
    (progn
      (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 110))) ; https://www.nerdfonts.com/font-downloads
  (t
    (progn
      (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 110)))) ; https://www.nerdfonts.com/font-downloads

;; Set default-directory
; Windows weird
(cond 
  ((eq `windows-nt system-type)
    (progn
      (setq default-directory (substitute-env-vars "$HOME/"))))
  (t
    (progn
	    (setq default-directory "~/"))))

;; Various Emacs Settings
(setq user-full-name "Ryder McMinn"
      user-mail-address "mcminnra@gmail.com")
(scroll-bar-mode -1)                                  ; No Scroll
(tool-bar-mode -1)                                    ; No Toolbar
(menu-bar-mode -1)                                    ; No Menu Bar
(setq inhibit-startup-screen t)                       ; No start screen
(show-paren-mode 1)                                   ; Make Emacs highlight paired parentheses
(setq visible-bell t)                                 ; Make bell visible
(setq backup-directory-alist `(("." . "~/.saves")))   ; Set backupdir
(toggle-frame-maximized)                              ; Set max window on startup (Mac OSX only?)
(setq create-lockfiles nil)                           ; Turn off .# lock files
(global-auto-revert-mode t)                           ; Auto refresh buffers

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95))
(add-to-list 'default-frame-alist '(alpha . (95)))

;; Emacs Helper functions
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; ===============================================
;; Packages Config
;; ===============================================
;; Theme
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-old-hope t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Helm
(use-package helm
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

;; multiple-cursors
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-M-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-M-<up>") 'mc/mark-previous-like-this))

;; ace-window
(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; evil mode
(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

;; ===============================================
;; Org Config
;; ===============================================
;; Org
(use-package org
  :mode (("\\.org$" . org-mode))
  ;; :ensure org-plus-contrib
  :hook (org-mode . org-indent-mode)
  :bind (("C-c n" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c l" . org-store-link))
  :config
  (add-to-list 'org-modules 'org-habit t)
  ;(define-key global-map "\C-ca" 'org-agenda)
  ;(define-key global-map "\C-cl" 'org-store-link)
  (setq org-log-done t)
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-deadline-warning-days 90)
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords
	'(
	  ; Tasks
	  (sequence "TODO(t)" "WAITING(w)" "BLOCKED(b)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)")
	  ; Project
          (sequence "PROJECT(p)" "FEATURE(f)" "BUG(u)" "IN-PROGRESS(i)" "|" "DONE(d)" "ARCHIVED(a)" "CANCELLED(c)")
	  ; Experience
          (sequence "EXPERIENCE(E)" "|" "ONE(1)" "TWO(2)" "THREE(3)" "FOUR(4)" "FIVE(5)" "ARCHIVED(a)")
	  ; Learning
	  (sequence "BACKLOG(L)" "IN-NOTEBOOK(O)" "IN-NPML(N)" "IN-REPO(R)" "WITH-NOTES(W)" "|" "ARCHIVED(A)")))
  (setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#30acec" :weight bold))
        ("WAITING" . (:foreground "#339989" :weight bold))
        ("IN-PROGRESS" . (:foreground "#725ac1" :weight bold))
        ("REVIEW" . (:foreground "#f7b801" :weight bold))
        ("BLOCKED" . (:foreground "#f6511d" :weight bold))
        ("DONE" . (:foreground "#6a994e" :weight bold))
        ("CANCELLED" . (:foreground "#d64a3b" :weight bold))
        ;; Project
        ("PROJECT" . (:foreground "white" :background "#5C3E84" :weight bold))
	("FEATURE" . (:foreground "#686EE2" :weight bold))
	("BUG" . (:foreground "#F35C6E" :weight bold))
        ;; Experience
        ("EXPERIENCE" . (:foreground "white" :background "orange" :weight bold))
        ("ONE" . (:foreground "red" :weight bold))
        ("TWO" . (:foreground "orange" :weight bold))
        ("THREE" . (:foreground "gold" :weight bold))
        ("FOUR" . (:foreground "lightgreen" :weight bold))
        ("FIVE" . (:foreground "forestgreen" :weight bold))
        ;; Learning Flow
        ("BACKLOG" . (:foreground "#30acec" :weight bold))
        ("IN-NOTEBOOK" . (:foreground "white" :background "#30acec" :weight bold))
        ("IN-NPML" . (:foreground "white" :background "purple" :weight bold))
        ("IN-REPO" . (:foreground "white" :background "#7cb518" :weight bold))
        ("WITH-NOTES" . (:foreground "white" :background "#a47e1b" :weight bold))))
  (setq org-enable-priority-commands t
	org-highest-priority ?A
	org-default-priority ?B
	org-lowest-priority ?C)
  (setq org-priority-faces
	'((?A . (:foreground "green"))
	  (?B . (:foreground "DeepSkyBlue"))
	  (?C . (:foreground "yellow"))
	  (?D . (:foreground "orange red"))
          (?E . (:foreground "red"))))
  ;; agenda
  (setq org-agenda-files (directory-files-recursively "~/Dropbox/" "\\.org$"))
  (setq org-agenda-deadline-faces
	'((0.92 . org-warning)
          (0.84 . org-upcoming-deadline)
          (0.0 . default)))
  (setq org-agenda-sorting-strategy
	'((agenda habit-down deadline-up scheduled-up time-up priority-down category-keep)
          (todo priority-down category-keep todo-state-up)
          (tags priority-down category-keep)
          (search category-keep)))
  (setq org-agenda-start-on-weekday nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets
	`((,(directory-files-recursively "~/Dropbox/" "^[a-zA-Z0-9_]*.org$") :maxlevel . 5)))
  (setq org-outline-path-complete-in-steps nil)
  ;; habit
  (setq org-habit-show-all-today t)
  (setq org-habit-following-days 1)
  (setq org-habit-preceding-days 29)
  (setq org-habit-graph-column 50)
  ;; capture
  (setq org-capture-templates
	'(("l" "Link" entry (file "~/Dropbox/org/capture.org") "* [[%^{link-url}][%^{link-description}]]")
          ("t" "Tasks" entry (file  "~/Dropbox/org/capture.org") "* TODO %?\n %U")))
  ;; functions
  (defun org-cycle-agenda-files ()
    "Cycle through the files in `org-agenda-files'. If the current buffer visits an agenda file, find the next one in the list. If the current buffer does not, find the first agenda file."
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
      (if (buffer-base-buffer) (org-pop-to-buffer-same-window (buffer-base-buffer))))))

(use-package org-edna
  :config
  (org-edna-mode))

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
	'(
	  (:name "Pursuits" :file-path "phel.org")
	  (:name "Work Tasks"
		 :and (:file-path "work_sie.org" :scheduled today :not (:habit t)))
	  (:name "Personal Tasks"
		 :and (:file-path "tasks.org" :scheduled today :not (:habit t)))
	  (:name "Habits"
		 :habit t)
	  (:name "Next"
		 :anything)))
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))  
