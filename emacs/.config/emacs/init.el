;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;; Author: Ryder McMinn
;; Keywords: lisp, config

;;; Commentary:
;; This is my personal Emacs configuration, optimized for Gnome/Linux.

;;; Code:
(when (version< emacs-version "30") (error "This requires Emacs 30 and above!"))

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

;; Set default-directory
(cond
 ((eq `windows-nt system-type)
  (progn
    (setq default-directory (substitute-env-vars "$HOME/")))) ; Windows weird
 (t
  (progn
    (setq default-directory "~/"))))

;; UI
(scroll-bar-mode -1)                                         ; No scroll bar
(tool-bar-mode -1)                                           ; No toolbar
(menu-bar-mode -1)                                           ; No menu bar
(setq inhibit-startup-screen t)                              ; No start screen
(global-hl-line-mode 1)                                      ; highlight active line
(toggle-frame-maximized)                                     ; Set max window on startup (Mac OSX only?)
(show-paren-mode 1)                                          ; Make Emacs highlight paired parentheses
(setq visible-bell t)                                        ; Make bell visible
(add-to-list 'default-frame-alist '(undecorated . t))        ; Remove title-bar

;; Editor behavior
(setq backup-directory-alist `(("." . "~/.saves")))          ; Set backupdir
(setq create-lockfiles nil)                                  ; Turn off .# lock files
(global-auto-revert-mode t)                                  ; Auto refresh buffers
(add-hook 'text-mode-hook #'visual-line-mode)                ; Turn on visual mode for text

;; Window management: only side-by-side splits, max 2 main windows + sidebar
(setq split-height-threshold nil)                              ; Never split top/bottom
(setq split-width-threshold 80)                                ; Allow side-by-side when wide enough

;; Indentation
(setq-default indent-tabs-mode nil)                          ; Use spaces, not tabs
(setq-default tab-width 4)                                   ; 4-space indentation
(setq-default sgml-basic-offset 4)                           ; HTML/SGML indent
(setq-default css-indent-offset 4)                           ; CSS indent
(setq-default js-indent-level 4)                             ; JS indent

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

;; avy (visual jump)
(use-package avy
  :ensure t
  :bind
  (("C-;" . avy-goto-char-timer)
   ("C-'" . avy-goto-line)))

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
;; Bytemancer
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
(load-theme 'bytemancer t)

(use-package nerd-icons)

;; (use-package doom-themes
;;   :ensure t
;;   :custom
;;   ;; Global settings (defaults)
;;   (doom-themes-enable-bold t)
;;   (doom-themes-enable-italic t)
;;   ;; for treemacs users
;;   ;;(doom-themes-treemacs-theme "doom-atom")
;;   :config
;;   ;(load-theme 'doom-old-hope t)
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Treemacs theme
;;   ;;(doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; emacs-dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-startup-banner 2)
  (setq dashboard-banner-logo-title "")
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)))
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

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
;;; Core Functionality
;;; ===============================================
;; Vertico
;; Minibuffer completion
(use-package vertico
  :custom
  (vertico-count 20)
  :init
  (vertico-mode))

;; Corfu
;; In-Buffer completions
(use-package corfu
  :init
  (global-corfu-mode))

;; Cape
;; Completion at point extensions (extends corfu basically)
(use-package cape
  :bind ("M-p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; Marginalia
;; Enable rich annotations
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Orderless
;; Better completion method
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))

;; Consult
;; Better search and navigation commands
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ("C-c b u" . my/consult-diff-unsaved-buffers-live)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-org-heading)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ([remap isearch-forward] . consult-line)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (defun my/consult-diff-unsaved-buffers-live ()
    "Select an unsaved buffer with live DIFF preview, then jump to first change."
    (interactive)
    (let* ((unsaved-buffers
            (cl-remove-if-not
             (lambda (buf)
               (and (buffer-file-name buf)
                    (buffer-modified-p buf)))
             (buffer-list)))
           (candidates (mapcar #'buffer-name unsaved-buffers)))
      (if (null candidates)
          (message "No unsaved buffers.")
	(when-let ((buf (consult--read
			 candidates
			 :prompt "Diff unsaved buffer: "
			 :category 'buffer
			 :require-match t
			 :state (lambda (action cand)
                                  (when (and cand (eq action 'preview))
                                    (diff-buffer-with-file (get-buffer cand))))
			 :sort nil)))
          (my/goto-first-diff (get-buffer buf))))))

  (defun my/goto-first-diff (buffer)
    "Switch to BUFFER and move point to the first line differing from file."
    (let* ((file (buffer-file-name buffer))
           (file-lines (with-temp-buffer
			 (insert-file-contents file)
			 (split-string (buffer-string) "\n")))
           (buf-lines (with-current-buffer buffer
			(split-string (buffer-string) "\n")))
           (line-num 1))
      (switch-to-buffer buffer)
      (catch 'found
	(while (or file-lines buf-lines)
          (unless (equal (car file-lines) (car buf-lines))
            (goto-char (point-min))
            (forward-line (1- line-num))
            (throw 'found t))
          (setq file-lines (cdr file-lines)
		buf-lines (cdr buf-lines)
		line-num (1+ line-num)))))))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/repos"))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

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
  (global-set-key (kbd "M-o") 'ace-window)
  ;; Make ace-window always respect the no-other-window parameter (e.g. dirvish-side)
  (defun my/aw-ignore-no-other-window (original-fn window)
    (or (funcall original-fn window)
        (window-parameter window 'no-other-window)))
  (advice-add 'aw-ignored-p :around #'my/aw-ignore-no-other-window))

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
;; Dirvish (file tree sidebar)
(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dired-listing-switches "-Al --group-directories-first")
  (setq dirvish-attributes '(nerd-icons file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-side-width 35)
  (dirvish-side-follow-mode 1)

  (defun my/dirvish-mouse-click (event)
    "Smart mouse handler: toggle subtree for dirs, open files in other window."
    (interactive "e")
    (mouse-set-point event)
    (when-let ((filename (dired-get-filename nil t)))
      (if (file-directory-p filename)
          (dirvish-subtree-toggle)
        (when-let ((target (get-mru-window nil nil t)))
          (select-window target)
          (find-file filename)))))

  (define-key dirvish-mode-map [follow-link] nil)

  :bind
  (("C-c t t" . dirvish-side)
   ("C-c t d" . dirvish)
   :map dirvish-mode-map
   ("TAB" . dirvish-subtree-toggle)
   ([mouse-1] . my/dirvish-mouse-click)))

(add-hook 'projectile-after-switch-project-hook
          (lambda ()
            (when (projectile-project-p)
              (dirvish-side)
              (other-window 1))))

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

;; Flycheck
(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :config
  (which-key-add-key-based-replacements "C-c e" "Errors (Flycheck)")
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
           typescript-ts-mode
           tsx-ts-mode
           rust-ts-mode
           go-ts-mode
           c-ts-mode
           c++-ts-mode
           bash-ts-mode
           css-ts-mode
           html-ts-mode
           json-ts-mode
           yaml-ts-mode
           dockerfile-ts-mode
           cmake-ts-mode
           lua-ts-mode
           svelte-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (which-key-add-key-based-replacements "C-c l" "LSP")
  (setq lsp-prefer-flymake nil)  ; Use flycheck instead of flymake
  (setq lsp-auto-guess-root t)
  (setq lsp-enable-suggest-server-download t)
  (setq lsp-auto-install-server t)

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

;; Svelte (requires typescript-mode for <script lang="ts"> block highlighting)
(use-package typescript-mode
  :ensure t
  :defer t
  :config
  (setq typescript-indent-level 4))

(use-package svelte-mode
  :ensure t
  :after typescript-mode
  :mode "\\.svelte\\'"
  :config
)

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
  (which-key-add-key-based-replacements "C-c g" "Git Diff")
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

;; Treesit-auto (auto-install grammars and remap modes — Mason-like experience)
(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Treesit config
(setq treesit-font-lock-level 4)   ; Most detailed syntax highlighting
(add-hook 'prog-mode-hook #'hs-minor-mode)  ; Enable tree-sitter based folding

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
  :demand t
  :mode (("\\.org$" . org-mode))
  ;; :ensure org-plus-contrib
  :hook (org-mode . org-indent-mode)
  :hook (org-agenda-finalize-hook . org-habit-streak-percentage)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
	     ("C-c u" . my/iue-update))
  :config
  (define-key org-mode-map (kbd "C-'") nil)  ; Unbind org-cycle-agenda-files; C-' used for avy-goto-line
  (add-to-list 'org-modules 'org-habit t)

  ;; Functions
  (defun org-habit-streak-percentage ()
    (goto-char (point-min))
    (while (not (eobp))
      (when (get-text-property (point) 'org-habit-p)
	(let* ((count (count-matches
                       (char-to-string org-habit-completed-glyph)
                       (line-beginning-position) (line-end-position)))
               (total-days (+ org-habit-following-days org-habit-preceding-days))
               (percentage (/ (round (* 10 (* 100 (/ (float count) total-days)))) 10.0))
               (percentage-str (concat " " (number-to-string percentage) "%"))
               (face (cond
                      ((>= percentage 66.6) 'success)
                      ((>= percentage 33.3) 'warning)
                      (t 'error))))
          (end-of-line)
          (insert (propertize percentage-str 'face face))))
      (forward-line 1)))

  ;; IUE prompt - returns formatted [T-IU|E] string
  (defun my/iue-prompt ()
    "Prompt for I, U, E scores and return formatted [T-IU|E] string.
       Importance: 1 (low) to 3 (high)
       Urgency:    1 (low) to 3 (high)
       Ease:       1 (hard/long) to 3 (easy/quick)
     Priority is derived from I+U only. E is for scheduling."
    (let* ((i (string-to-number (read-string "Importance (1-3): ")))
           (u (string-to-number (read-string "Urgency (1-3): ")))
           (e (string-to-number (read-string "Ease (1-3): ")))
           (total (+ i u)))
      (format "[%d-%d%d|%d]" total i u e)))

  ;; Sync priority cookie from IUE token
  (defun my/iue-sync-priority ()
    "Read [T-IU|E] token on current headline and set org priority accordingly.
     Priority is based on I+U total (range 2-6)."
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "\\[\\([0-9]+\\)-[0-9]\\{2\\}|[0-9]\\]" (line-end-position) t)
	(let* ((total (string-to-number (match-string 1)))
               (priority (pcase total
                           (6 ?A) (5 ?B) (4 ?C)
                           (3 ?D) (2 ?E) (_ ?E))))
          (org-priority priority)))))

  (defun my/iue-update ()
    "Prompt for IUE scores, update token on headline, and sync priority."
    (interactive)
    (let ((new (my/iue-prompt)))
      (save-excursion
	(org-back-to-heading t)
	;; First remove any existing IUE token (and its trailing space)
	(when (re-search-forward "\\[[0-9]+-[0-9]\\{2\\}|[0-9]\\] ?" (line-end-position) t)
          (replace-match ""))
	;; Now insert at the right position: after stars, TODO, and priority
	(beginning-of-line)
	(if (re-search-forward "^\\*+ \\(?:TODO\\|DONE\\|[A-Z-]+\\)? ?\\(?:\\[#[A-G]\\]\\)? ?"
                               (line-end-position) t)
            (insert new " ")
          (end-of-line)
          (insert " " new))))
    (my/iue-sync-priority))

  ;; Extract IUE scores from current line
  (defun my/iue-get-scores ()
    "Extract IUE scores from current line. Returns (total importance urgency ease).
     Returns (0 0 0 0) if no token is found, sorting unscored items to the bottom."
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "\\[\\([0-9]\\)-\\([0-9]\\)\\([0-9]\\)|\\([0-9]\\)\\]"
                             (line-end-position) t)
          (list (string-to-number (match-string 1))
                (string-to-number (match-string 2))
                (string-to-number (match-string 3))
                (string-to-number (match-string 4)))
        '(0 0 0 0))))

  ;; Custom comparison function for agenda sorting
  (defun my/iue-compare (a b)
    "Compare two agenda entries by IUE scores.
     Returns -1 if a < b, +1 if a > b, nil if equal.
     Sorts by total (I+U), then importance, then urgency, then ease."
    (let ((sa (get-text-property 0 'txt a))
          (sb (get-text-property 0 'txt b)))
      (let ((scores-a (with-temp-buffer
			(insert sa)
			(my/iue-get-scores)))
            (scores-b (with-temp-buffer
			(insert sb)
			(my/iue-get-scores))))
	(cl-loop for val-a in scores-a
                 for val-b in scores-b
                 when (> val-a val-b) return +1
                 when (< val-a val-b) return -1
                 finally return nil))))

  ;; General Org config
  (setq org-ellipsis " ▼ ")                                 ; Change icon for folding
  (setq org-todo-repeat-to-state "TODO")                    ; Recurring tasks reset to TODO, not first state in sequence
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
          (sequence "WAITING(w)" "DEFERRED(e)" "TODO(t)" "PLANNED(p)" "IN-PROGRESS(i)" "REVIEW(r)" "|" "CANCELLED(c)" "DONE(d)")
          ;; Project
          (sequence "PROJECT(P)" "FEATURE(f)" "BUG(u)" "IN-PROGRESS(i)" "|" "CANCELLED(c)" "DONE(d)" "ARCHIVED(a)")
          ;; Experience
          (sequence "EXPERIENCE(E)" "|" "ONE(1)" "TWO(2)" "THREE(3)" "FOUR(4)" "FIVE(5)" "ARCHIVED(a)")
          ;; Learning
          (sequence "BACKLOG(L)" "IN-NOTEBOOK(O)" "IN-NPML(N)" "IN-REPO(R)" "WITH-NOTES(W)" "|" "ARCHIVED(A)")))
  (setq org-todo-keyword-faces
        '(("IN-PROGRESS" . (:foreground "#725ac1" :weight bold))
          ("TODO" . (:foreground "#30acec" :weight bold))
          ("PLANNED" . (:foreground "#a08cd8" :weight bold))
          ("REVIEW" . (:foreground "#f7b801" :weight bold))
          ("WAITING" . (:foreground "#339989" :weight bold))
          ("DEFERRED" . (:foreground "#9d9d9d" :weight bold))
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

  ;; Set the custom comparison function
  (setq org-agenda-cmp-user-defined #'my/iue-compare)
  (setq org-agenda-sorting-strategy
        '((agenda habit-up scheduled-up deadline-up time-up todo-state-down user-defined-down category-keep)
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
  (setq org-habit-show-done-always-green t))

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
	  (:name "Focus (Project+Media)" :tag "focus")
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
;; Window management
(defun my/main-window-count ()
  "Count non-side-panel (main) windows."
  (length (seq-filter
           (lambda (w) (not (window-parameter w 'window-side)))
           (window-list))))

(defun my/split-window-sensibly (&optional window)
  "Only split right, only when <2 main windows. Used internally by `display-buffer'."
  (let ((window (or window (selected-window))))
    (when (and (< (my/main-window-count) 2)
               (window-splittable-p window t))
      (with-selected-window window
        (split-window-right)))))

(defun my/split-right ()
  "Split window right and follow, capped at 2 main windows."
  (interactive)
  (if (>= (my/main-window-count) 2)
      (message "Already at max 2 main windows")
    (split-window-right)
    (other-window 1)))

(setq split-window-preferred-function #'my/split-window-sensibly)
(setq display-buffer-base-action
      '((display-buffer-reuse-window display-buffer-use-some-window)))
(global-set-key (kbd "C-x 2") #'my/split-right)
(global-set-key (kbd "C-x 3") #'my/split-right)

(defun my/open-org-layout ()
  "Open org layout: dirvish sidebar | tasks file | day agenda."
  (interactive)
  (delete-other-windows)
  (find-file "~/org/notes/project-tasks.org")
  (split-window-right)
  (other-window 1)
  (org-agenda-list)
  (org-agenda-day-view)
  (other-window -1)
  (dirvish-side)
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
