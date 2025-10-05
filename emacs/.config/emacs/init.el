;;; init.el -- Emacs Config

(when (version< emacs-version "29") (error "This requires Emacs 29 and above!"))

;; Load package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t) ; Have use-package also invoke straight.el

;;; ===============================================
;;; General Emacs Settings
;;; ===============================================
(setq user-full-name "Ryder McMinn"
      user-mail-address "rdr@rdrmc.com")

;; Fonts
(when (member "SauceCodePro Nerd Font Mono" (font-family-list))
  (set-face-attribute 'default nil :font "SauceCodePro Nerd Font Mono" :height 100))
(when (member "SauceCodePro NFM" (font-family-list)) ; Unsure why on some systems it is shortened
  (set-face-attribute 'default nil :font "SauceCodePro NFM" :height 100))

;; Set default-directory
(cond
 ((eq `windows-nt system-type)
  (progn
    (setq default-directory (substitute-env-vars "$HOME/")))) ; Windows weird
 (t
  (progn
    (setq default-directory "~/"))))

(scroll-bar-mode -1)                                         ; No scroll bar
(tool-bar-mode -1)                                           ; No toolbar
(menu-bar-mode -1)                                           ; No menu bar
(setq inhibit-startup-screen t)                              ; No start screen
(global-hl-line-mode 1)                                      ; highlight active line
(toggle-frame-maximized)                                     ; Set max window on startup (Mac OSX only?)
(show-paren-mode 1)                                          ; Make Emacs highlight paired parentheses
(setq visible-bell t)                                        ; Make bell visible
(setq backup-directory-alist `(("." . "~/.saves")))          ; Set backupdir
(setq create-lockfiles nil)                                  ; Turn off .# lock files
(global-auto-revert-mode t)                                  ; Auto refresh buffers
;(setq split-width-threshold 80)                              ; lower the threshold to automatically split vertically
;(setq split-height-threshold nil)                            ; --^
(add-hook 'text-mode-hook #'visual-line-mode)                ; Turn on visual mode for text
(add-to-list 'default-frame-alist '(undecorated . t))        ; Remove title-bar

;; Less jumpy mouse scroll
;; Stolen From: https://github.com/deirn/fedoracfg/blob/deadb8eef399ef563e76f97edfcd9120643d0fc0/config/emacs/init.el#L122
(setq scroll-step 1)                      ; Scroll one line at a time
(setq scroll-margin 2)                    ; Start scrolling when 2 lines from the bottom
(setq scroll-conservatively 100000)       ; Never re-center automatically
(setq scroll-preserve-screen-position t)  ; Keep point position when scrolling
(setq auto-window-vscroll nil)

;; Config line numbers
(use-package display-line-numbers
  :ensure nil  ; Comes preloaded with emacs
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (display-line-numbers-type t)
  :hook
  (prog-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode))

;; Windmove
;; -- Arrow key window movement
(use-package windmove
  :ensure nil  ; windmove is built-in
  :bind*
  (("C-c <left>" . windmove-left)
   ("C-c <right>" . windmove-right)
   ("C-c <up>" . windmove-up)
   ("C-c <down>" . windmove-down)))

;; OS-specific settings
(cond
  ((eq `gnu/linux system-type)
   (progn
     (set-frame-parameter nil 'alpha-background 98)              ; Transparency
     (add-to-list 'default-frame-alist '(alpha-background . 98)) ; Transparency
     ))
  ((eq `darwin system-type)
   (progn
     (setq mac-command-modifier 'meta)                        ; Setup cmd key as "alt" on mac
     )))

;;; ===============================================
;;; Theming
;;; ===============================================
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  ;; Install fonts automatically if they're not already installed
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(add-to-list 'custom-theme-load-path
               (expand-file-name "themes" user-emacs-directory))
(load-theme 'bytemancer t)

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-colors")
  :config
  ;(load-theme 'doom-old-hope t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Treemacs theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Solaire
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

;; visual-line-column
(use-package visual-fill-column
  :ensure t
  :hook ((org-mode . visual-fill-column-mode)
         (markdown-mode . visual-fill-column-mode))
  :config
  (setq-default visual-fill-column-width 120)
  (setq-default visual-fill-column-center-text t)
  
  ;; Optional: Enable visual-line-mode with visual-fill-column
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;;; ===============================================
;;; Productivity
;;; ===============================================
;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/repos"))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; Helm
(use-package helm
  :ensure t
  :config
  ; Force helm to search inside of a given window
  (setq helm-split-window-inside-p t) 
  ; Keybinds
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-s") #'helm-occur)
  (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :init
  (helm-projectile-on))

;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  ;; Core multiple-cursors bindings
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  ;; Click to add cursor
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
  ;; Line editing
  (global-set-key (kbd "C-S-c C-S-a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-S-c C-S-e") 'mc/edit-ends-of-lines))

;; ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; transpose-frame
;; NOTE: in emacs 31+, this is now native - Switch to those when upgrading
(use-package transpose-frame
  :ensure t
  :bind (("C-x 7 t" . transpose-frame)
         ("C-x 7 f" . flip-frame)
         ("C-x 7 r" . rotate-frame)))

;; ;; Evil mode
;; (use-package evil
;;   :ensure t
;;   :demand t
;;   :init
;;   (setq evil-want-C-i-jump nil)
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1)
;;   ; Revert to trad emacs keybinds for these
;;   (define-key evil-motion-state-map (kbd "C-a") 'move-beginning-of-line)
;;   (define-key evil-motion-state-map (kbd "C-e") 'move-end-of-line)
;;   ; Evil mode navigate between visual lines insted of logical
;;   (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
;;   (define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)
;;   (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
;;   (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
;;   ; Make vim use org mode heading cmds in org-mode
;;   (evil-define-key 'normal org-mode-map
;;     (kbd "]]") 'org-next-visible-heading
;;     (kbd "[[") 'org-previous-visible-heading))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))

;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :hook (org-mode . (lambda () evil-org-mode))
;;   :config
;;   (require 'evil-org-agenda)
;;   (evil-org-agenda-set-keys))

;; Treemacs
(use-package treemacs
  :ensure t
  :defer t
  ;; :hook (treemacs-mode . (lambda () (treemacs-resize-icons 16)))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    ; Prevent other windows using treemacs window
    (setq treemacs-is-never-other-window nil)
    (setq treemacs-position 'left) 
    (setq treemacs-width 35)
    ; Make treemacs automatically follow the file you are viewing
    (treemacs-follow-mode t)
    ; Automatically update the tree when files change on disk
    (treemacs-filewatch-mode t)
    ; Enable git integration for status highlights
    (treemacs-git-mode 'deferred))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t d"   . treemacs-select-directory)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

;; (use-package treemacs-evil
;;   :ensure t
;;   :after (treemacs evil))

;; ===============================================
;; Programming 
;; ===============================================
;; Rainbow Delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Smartparens
(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; eat (Emulate A Terminal)
(use-package eat
  :ensure t
  :config
  (setq eat-kill-buffer-on-exit t)
  (setq eat-enable-mouse t)
  :bind
  ("C-c RET" . eat))

;; Company
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

;; Flycheck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :bind (("C-c e n" . flycheck-next-error)
         ("C-c e p" . flycheck-previous-error)
         ("C-c e l" . flycheck-list-errors)))

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (((python-ts-mode
           js-ts-mode
	   elisp-ts-mode
           typescript-ts-mode
           rust-ts-mode
           go-ts-mode
           c-ts-mode
           c++-ts-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)  ; Use flycheck instead of flymake
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-suggest-server-download t)
  
  (setq lsp-completion-provider :capf)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  
  ;;; Python
  ;; Register ruff LSP server
  (lsp-register-client
   (make-lsp-client 
    :new-connection (lsp-stdio-connection '("ruff" "server" "--preview"))
    :activation-fn (lsp-activate-on "python")
    :server-id 'ruff
    :add-on? t
    :priority -1)))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-ts-mode . (lambda () (lsp))))  ; or lsp-deferred

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-sideline-enable t
	lsp-ui-sideline-show-hover t
	lsp-ui-doc-enable nil
	lsp-ui-sideline-show-diagnostics t     ; Show warnings/errors in sideline
	lsp-ui-sideline-show-code-actions nil
	lsp-ui-sideline-delay 0.25)    ; Show code actions in sideline
  )

(use-package helm-lsp
  :ensure t
  :after (helm lsp-mode)
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; diff-hl
(use-package diff-hl
  :ensure t
  :hook
  ;; Enable diff-hl for programming modes
  ((prog-mode . diff-hl-mode)
   (vc-dir-mode . diff-hl-dir-mode)
   ;; Enable margin mode for terminal Emacs
   (diff-hl-mode . diff-hl-margin-mode))
  :config
  ;; Enable diff-hl globally
  (global-diff-hl-mode 1)
  
  ;; Refresh diff-hl when magit operations complete
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  
  ;; Use margin instead of fringe in terminal
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  
  :bind
  ;; Navigate between hunks
  ("C-c g n" . diff-hl-next-hunk)
  ("C-c g p" . diff-hl-previous-hunk)
  ;; Show diff of current hunk
  ("C-c g s" . diff-hl-show-hunk)
  ;; Revert current hunk
  ("C-c g r" . diff-hl-revert-hunk))

;; Treesit
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (lua "https://github.com/Azganoth/tree-sitter-lua")))

;; Remap major modes to their tree-sitter equivalents
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
	(elisp-mode . elisp-ts-mode)
        (html-mode . html-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (cxx-mode . c++-ts-mode)
        (cc-mode . c++-ts-mode)
        (rust-mode . rust-ts-mode)
        (go-mode . go-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (toml-mode . toml-ts-mode)))

;; Auto-mode mappings for file extensions
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))

;; Treesit config
(setq treesit-font-lock-level 4)   ; Most detailed syntax highlighting
(add-hook 'prog-mode-hook #'hs-minor-mode)  ; Enable tree-sitter based folding

;; Function to install all grammar files
(defun my/install-tree-sitter-grammars ()
  "Install all tree-sitter grammars defined in `treesit-language-source-alist'."
  (interactive)
  (dolist (lang treesit-language-source-alist)
    (let ((lang-symbol (car lang)))
      (unless (treesit-language-available-p lang-symbol)
        (message "Installing %s grammar..." lang-symbol)
        (treesit-install-language-grammar lang-symbol)))))
(add-hook 'prog-mode-hook 'my/install-tree-sitter-grammars)  ; Install on prog-modes

;; Make comments italic
(custom-set-faces
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-comment-delimiter-face ((t (:slant italic)))))

;;; ===============================================
;;; Org Config
;;; ===============================================
;; Org
(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  ;; :ensure org-plus-contrib
  :hook (org-mode . org-indent-mode)
  :hook (org-agenda-finalize-hook . org-habit-streak-count)
  :bind (("C-c o c" . org-capture)
         ("C-c o a" . org-agenda)
         ("C-c o l" . org-store-link))
  :config
  (add-to-list 'org-modules 'org-habit t)

  ;; General Org config
  (setq org-ellipsis " ▼ ")                                 ; Change icon for folding
  (setq org-log-done t)                                     ; Record when done
  (setq org-log-into-drawer "LOGBOOK")                      ; Put log state into a seperate section instead of just in headline body 
  (setq org-enforce-todo-dependencies t)                    ; Can't mark done if children aren't marked done
  (setq org-archive-location "~/org/archive/%s_archive::")  ; Set archive location and format
  (setq org-tags-column 0)                                  ; Put tags immediately after headline
  (setq org-special-ctrl-a/e t)                             ; Have ctrl-a/e work better with org headlines
  
  ;; But make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font "SauceCodePro NFM" :weight 'bold :height 1.5)

  ;; Set todo keywords and faces
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

  ;; Org priority and faces
  (setq org-enable-priority-commands t
        org-highest-priority ?A
        org-default-priority ?G
        org-lowest-priority ?G)
  (setq org-priority-faces  ; Using WoW quality colors because F it - https://warcraft.wiki.gg/wiki/Quality
        '((?A . (:foreground "#e6cc80"))
          (?B . (:foreground "#ff8000"))
          (?C . (:foreground "#a335ee"))
          (?D . (:foreground "#0070dd"))
          (?E . (:foreground "#1eff00"))
          (?F . (:foreground "#ffffff"))
          (?G . (:foreground "#9d9d9d"))))

  ;; Set todo warning and faces
  (setq org-deadline-warning-days 90)
  (setq org-agenda-deadline-faces
        '((0.98 . org-imminent-deadline)          ; Overdue or due tomorrow
          (0.66 . org-upcoming-deadline)          ; Due within 30 days 
          (0.0 . org-upcoming-distant-deadline))) ; everything else

  ;; agenda
  (setq org-agenda-files (directory-files-recursively "~/org/notes/" "\\.org$"))  ; Recursively find all files
  (setq org-agenda-dim-blocked-tasks nil)  ; If a tasks has sub-tasks, it gets dimmed, which I dislike
  (setq org-agenda-start-on-weekday nil)  ; Start weekday on Monday
  (setq org-agenda-remove-tags t)  ; hide tags in agenda

  (setq org-agenda-prefix-format
        '((agenda . " %i %-24:c%?-24t%s")
          (todo . " %i %-24:c")
          (tags . " %i %-24:c")
          (search . " %i %-24:c")))

  (setq org-agenda-sorting-strategy
        '((agenda habit-up scheduled-up deadline-up time-up todo-state-down priority-down category-keep)
          (todo priority-down category-keep todo-state-down)
          (tags priority-down category-keep)
          (search category-keep)))

  (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)  ; Refile in single go
  (setq org-refile-use-outline-path 'file)  ; Show full path starting with file

  ;; habit
  (setq org-habit-show-all-today t)
  (setq org-habit-following-days 1)
  (setq org-habit-preceding-days 29)
  (setq org-habit-graph-column 65)
  (setq org-habit-show-done-always-green t)

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
  :ensure t
  :after org
  :config
  (org-edna-mode))

(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (setq org-super-agenda-groups
	'(
	  (:name "Work" :tag "work")
	  (:name "Personal Focus" :and(:tag "personal" :tag "focus")) 
	  (:name "Personal" :tag "personal") 
	  (:name "Next" :anything)))
  (setq org-super-agenda-header-map (make-sparse-keymap))
  (org-super-agenda-mode))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("■" "□" "▫" "▫" "▫" "▫")))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/org/notes/")) ; Set the directory where your Org-roam files will be stored
  (org-roam-capture-templates
   '(("k" "knowledge" plain "%?"
      :target (file+head "knowledge-${slug}.org" "#+TITLE: Knowledge - ${title}\n#+STARTUP: showeverything\n#+FILETAGS: :personal:knowledge:\n")
      :unarrowed t)
     ("p" "project" plain "%?"
      :target (file+head "project-${slug}.org" "#+TITLE: Project - ${title}\n#+STARTUP: show2levels\n#+FILETAGS: :personal:project:\n")
      :unarrowed t)
     ("K" "work-knowledge" plain "%?"
      :target (file+head "work-knowledge-${slug}.org" "#+TITLE: Work Knowledge - ${title}\n#+STARTUP: showeverything\n#+FILETAGS: :work:knowledge:\n")
      :unarrowed t)
     ("P" "work-project" plain "%?"
      :target (file+head "work-project-${slug}.org" "#+TITLE: Work Project - ${title}\n#+STARTUP: showeverything\n#+FILETAGS: :work:project:\n")
      :unarrowed t)
     ))
  :bind
  (("C-c n l" . org-roam-buffer-toggle) ; Toggle the Org-roam buffer (shows backlinks, etc.)
   ("C-c n f" . org-roam-node-find)     ; Find an existing Org-roam node or create a new one
   ("C-c n i" . org-roam-node-insert)   ; Insert a link to an Org-roam node
   ("C-c n g" . org-roam-graph)         ; Display the Org-roam graph
   ("C-c n c" . org-roam-capture))      ; Capture a new Org-roam node using a template
  :config
  (org-roam-setup))

;;; ===============================================
;;; Functions
;;; ===============================================
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

(defun my/open-org-layout ()
  (interactive)
  (pop-to-buffer (find-file "~/org/notes/project-tasks.org"))
  (org-agenda-list)
  (org-agenda-day-view)
  (transpose-frame)
  (balance-windows)
  (treemacs-add-and-display-current-project-exclusively)
  (other-window 1))
(global-set-key (kbd "C-x 9") 'my/open-org-layout)

(defun collect-buffer-faces (buffer)
  "Collect all faces found in BUFFER"
  (let ((faces nil))
    (save-excursion
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (< (point) (point-max))
          (add-to-list 'faces (get-text-property (point) 'face))
          (goto-char (next-property-change (point) nil (point-max))))))
    (delete nil faces)))

;;;###autoload
(defun show-buffer-faces (&optional buffer)
  "Display faces used in the BUFFER in help window.

  If not specified, or called interactively, BUFFER defaults to `current-buffer'"
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (help-buffer (or (help-buffer) (get-bufer-create "*Help*")))
         (faces (collect-buffer-faces buffer))
         (help-buffer-under-preparation t))
    (help-setup-xref (list #'show-buffer-faces buffer)
		     (called-interactively-p 'interactive))
    (with-help-window help-buffer
      (insert
       (format "Faces found in buffer %s:\n\n" (buffer-name buffer)))

      (let ((sort-start (point)))
        (dolist (face faces)
          (help-insert-xref-button
           (propertize (format "  %s" face) 'face face)
           'help-face face)
          (insert "\n"))
        (sort-lines t sort-start (point))))))


;;; ----------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
