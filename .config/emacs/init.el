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
; Unsure why on some systems it is shortened
(when (member "SauceCodePro Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :font "SauceCodePro Nerd Font Mono" :height 100))
(when (member "SauceCodePro NFM" (font-family-list))
  (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 100))

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
      user-mail-address "rdr@rdrmc.com")
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
(add-hook 'prog-mode-hook 'display-line-numbers-mode) ; turn on numbers for programming modes

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(100))
(add-to-list 'default-frame-alist '(alpha . (100)))

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

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Helm
(use-package helm
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode 1))

;; which-key
(use-package which-key
  :config
  (which-key-mode))

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
  (evil-mode 1)
  ;; Revert to trad emacs keybinds for these
  (define-key evil-motion-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-motion-state-map (kbd "C-e") 'move-end-of-line)
  ;; Make vim use org mode heading cmds in org-mode
  (evil-define-key 'normal org-mode-map
    (kbd "]]") 'org-next-visible-heading
    (kbd "[[") 'org-previous-visible-heading))

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 80)
  ;; (olivetti-minimum-body-width 72)
  ;; (olivetti-header-margin 5)

  ; NOTE: Currently setting this by .dir-locals-el
  ;;:hook
  ;;(org-mode . olivetti-mode)
  ;;(markdown-mode . olivetti-mode)
  ;;(text-mode . olivetti-mode)
  ;;(prog-mode . olivetti-mode)

  :bind
  ("C-c o" . olivetti-mode))

;; Ensure Olivetti respects visual-line-mode's wrapping (often desirable)
(with-eval-after-load 'olivetti
  (advice-add 'olivetti-set-width :before-while #'visual-line-mode))

;; ===============================================
;; Programming Modes Config
;; ===============================================
;; Markdown
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package poly-markdown
  :ensure t)

;; ===============================================
;; Org Config
;; ===============================================
;; Org
(use-package org
  :mode (("\\.org$" . org-mode))
  ;; :ensure org-plus-contrib
  :hook (org-mode . org-indent-mode)
  :hook (org-agenda-finalize-hook . org-habit-streak-count)
  :hook (org-mode . visual-line-mode)  ;; Enable word wrap in Org mode
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-c l" . org-store-link))
  :config
  (add-to-list 'org-modules 'org-habit t)

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font "SauceCodePro NFM" :weight 'bold :height 1.5)

  (setq org-log-done t)
  (setq org-ellipsis " ▼ ")
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-archive-location "~/org/archive/%s_archive::")
  (setq org-deadline-warning-days 365)
  (setq org-enforce-todo-dependencies t)
  (setq org-tags-column 0)
  (setq org-special-ctrl-a/e t)
  (setq org-todo-keywords
	'(
	  ;; Tasks
	  (sequence "TODO(t)" "BLOCKED(b)" "WAITING(w)" "IN-PROGRESS(i)" "REVIEW(r)" "|" "CANCELLED(c)" "DONE(d)")
	  ;; Project
          (sequence "PROJECT(p)" "FEATURE(f)" "BUG(u)" "IN-PROGRESS(i)" "|" "CANCELLED(c)" "DONE(d)" "ARCHIVED(a)")
	  ;; Experience
          (sequence "EXPERIENCE(E)" "|" "ONE(1)" "TWO(2)" "THREE(3)" "FOUR(4)" "FIVE(5)" "ARCHIVED(a)")
	  ;; Learning
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
	org-default-priority ?D
	org-lowest-priority ?G)
  ; Using WoW quality colors because F it - https://warcraft.wiki.gg/wiki/Quality
  (setq org-priority-faces
	'((?A . (:foreground "#e6cc80"))
	  (?B . (:foreground "#ff8000"))
	  (?C . (:foreground "#a335ee"))
	  (?D . (:foreground "#0070dd"))
          (?E . (:foreground "#1eff00"))
	  (?F . (:foreground "#ffffff"))
	  (?G . (:foreground "#9d9d9d"))))
  ;; agenda
  (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (setq org-agenda-dim-blocked-tasks nil)  ; If a tasks has sub-tasks, it gets dimmed, which I dislike
  (setq org-agenda-deadline-faces
	'((0.92 . org-warning)
          (0.84 . org-upcoming-deadline)
          (0.0 . default)))
  (setq org-agenda-sorting-strategy
	'((agenda habit-down deadline-up scheduled-up time-up todo-state-down priority-down category-keep)
          (todo priority-down category-keep todo-state-down)
          (tags priority-down category-keep)
          (search category-keep)))
  (setq org-agenda-start-on-weekday nil)
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets
	`((,(directory-files-recursively "~/org/" "^[a-zA-Z0-9_]*.org$") :maxlevel . 5)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-agenda-prefix-format
	'((agenda . " %i %-12:c%?-12t% s %?-12T")
	  (todo . " %i %-12:c")
	  (tags . " %i %-12:c")
	  (search . " %i %-12:c")))
  (setq org-agenda-remove-tags t)
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
      (if (buffer-base-buffer) (org-pop-to-buffer-same-window (buffer-base-buffer)))))

  (defun org-habit-streak-percentage ()
    (point-min)
    (while (not (eobp))
      (when (get-text-property (point) 'org-habit-p)
        (let (
	      (count (count-matches
                      (char-to-string org-habit-completed-glyph)
                      (line-beginning-position) (line-end-position))))
          (end-of-line)
          (insert (concat (number-to-string
			   (/ (round (* 10 (* 100 (/ (float count) (+ org-habit-following-days org-habit-preceding-days)))))10.0)
			   ) "%" ))))
      (forward-line 1)))
  (add-hook 'org-agenda-finalize-hook 'org-habit-streak-percentage))

(use-package org-edna
  :config
  (org-edna-mode))

(use-package org-super-agenda
  :config
  (setq org-super-agenda-groups
	'(
	  (:name "Pursuits" :file-path ".notes/personal_northstar.org")
	  (:name "Projects" :tag "project")
	  (:name "Rules"
		 :habit t)
	  (:name "Work Tasks"
		 :and (:file-path "work_sie.org" :scheduled today :not (:habit t)))
	  (:name "Personal Tasks"
		 :and (:file-path "tasks.org" :scheduled today :not (:habit t))
		 :and (:file-path "/notes/" :scheduled today :not (:habit t))) 
	  (:name "Next"
		 :anything)))
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode))

(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("■" "□" "▫" "▫" "▫" "▫"))
  :hook (org-mode . org-superstar-mode))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/notes/")) ; Set the directory where your Org-roam files will be stored
  (org-roam-capture-templates
   '(("c" "concept" plain "%?"
      :target (file+head "concept-${slug}.org" "#+TITLE: ${title}\n#+STARTUP: showeverything\n#+FILETAGS: :concept:\n") :unarrowed t)
     ("p" "project" plain "%?"
      :target (file+head "project-${slug}.org" "#+TITLE: ${title}\n#+STARTUP: show2levels\n#+FILETAGS: :project:\n") :unarrowed t)
     ("l" "life" plain "%?"
      :target (file+head "life-${slug}.org" "#+TITLE: ${title}\n#+STARTUP: showeverything\n#+FILETAGS: :life:\n") :unarrowed t))
   )
  :bind
  (("C-c n l" . org-roam-buffer-toggle) ; Toggle the Org-roam buffer (shows backlinks, etc.)
   ("C-c n f" . org-roam-node-find)     ; Find an existing Org-roam node or create a new one
   ("C-c n i" . org-roam-node-insert)   ; Insert a link to an Org-roam node
   ("C-c n g" . org-roam-graph)         ; Display the Org-roam graph
   ("C-c n c" . org-roam-capture))      ; Capture a new Org-roam node using a template
  :config
  (org-roam-setup))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))  


;; ===============================================
;; Functions
;; ===============================================
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

(defun open-org-layout ()
  (interactive)
  (pop-to-buffer (find-file "~/org/tasks.org"))
  (org-agenda-list)
  (org-agenda-day-view)
  (treemacs-add-and-display-current-project-exclusively)
  (other-window 1))
(global-set-key (kbd "C-x 9") 'open-org-layout)
