;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Doom settings
(setq user-full-name "Ryder McMinn"
      user-mail-address "mcminnra@gmail.com")
(setq doom-theme 'doom-old-hope) ; doom-old-hope
(setq doom-font (font-spec :family "Source Code Pro" :weight 'normal))
(setq display-line-numbers-type t)
(setq org-directory "~/org/")

;; Set default Directory on Windows
(when (and (eq system-type 'windows-nt) (string= user-real-login-name "rymcminn"))
  (setq default-directory "c:/Users/rymcminn/" ))
(when (and (eq system-type 'windows-nt) (string= user-real-login-name "mcmin"))
  (setq default-directory "c:/Users/mcmin/" ))

;; Set Home on Windows
(when (eq system-type 'windows-nt)
  (setenv "Home" (getenv "UserProfile")))

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
(setq create-lockfiles nil)                           ; Turn off .# lock files

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95))
(add-to-list 'default-frame-alist '(alpha . (95)))

;; Helper functions
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
;; neotree
(global-set-key [f8] 'neotree-toggle)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ===============================================
;; Org Config
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
			     "~/org/l.org"
                             "~/org/capture.org"))
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
(setq org-agenda-start-on-weekday nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "IN-PROGRESS(i)" "REVIEW(r)" "BLOCKED(b)" "|" "DONE(d)" "CANCELLED(c)")
	(sequence "STORY->TODO(s)" "STORY->IN-PROGRESS(n)" "|" "STORY->DONE(y)")
	(sequence "HABIT(h)" "|" "DONE(f)")
	(sequence "REOCCURING(e)" "|" "DONE(g)")
	(sequence "HOBBY(H)" "PROJECT(P)" "EXPERIENCE(E)" "VIDEO-GAME(V)" "BOOK(B)" "|" "COMPLETED(C)" "1(1)" "2(2)" "3(3)" "4(4)" "5(5)")
	(sequence "BACKLOG(L)" "IN-NOTEBOOK(O)" "IN-NPML(N)" "IN-REPO(R)" "WITH-NOTES(W)" "|" "ARCHIVED(A)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#30acec" :weight bold))
	("WAITING" . (:foreground "#339989" :weight bold))
	("IN-PROGRESS" . (:foreground "#725ac1" :weight bold))
	("REVIEW" . (:foreground "#f7b801" :weight bold))
	("BLOCKED" . (:foreground "#f6511d" :weight bold))
	("DONE" . (:foreground "#6a994e" :weight bold))
	("CANCELLED" . (:foreground "#d64a3b" :weight bold))
	;; Story
	("STORY->TODO" . (:foreground "#008080" :weight bold))
	("STORY->IN-PROGRESS" . (:foreground "#725ac1" :weight bold))
	("STORY->DONE" . (:foreground "#6a994e" :weight bold))
	;; Reoccuring
	("REOCCURING" . (:foreground "#339989" :weight bold))
	;; Project, Hobby, Learning Flow
	("HOBBY" . (:foreground "white" :background "darkgreen" :weight bold))
	("VIDEO-GAME" . (:foreground "white" :background "darkblue" :weight bold))
	("BOOK" . (:foreground "white" :background "darkred" :weight bold))
	("PROJECT" . (:foreground "white" :background "purple" :weight bold))
	("EXPERIENCE" . (:foreground "white" :background "orange" :weight bold))
	("COMPLETED" . (:foreground "#80c34f" :weight bold))
	("1" . (:foreground "red" :weight bold))
        ("2" . (:foreground "orange" :weight bold))
        ("3" . (:foreground "gold" :weight bold))
        ("4" . (:foreground "lightgreen" :weight bold))
        ("5" . (:foreground "forestgreen" :weight bold))
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
