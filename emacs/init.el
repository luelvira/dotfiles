;;; init.el --- config file -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; This file has been generated from config.org file. DO NOT EDIT.
;; Sources are available from https://github.com/luelvira/dotfiles/

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "*** Emacs loaded in %s seconds with %d garbage collections."
             (emacs-init-time "%.2f")
             gcs-done)))

(setq straight-check-for-modifications nil)
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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering)

(setq backup-directory-alist '(("." . "~/.cache/emacs/backup/"))
  backup-by-copying t   ; instead of renaming current file (clobbers links)
  create-lockfiles nil
  make-backup-files t    ; Backup of a file the first time it is saved.
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 6   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t)

(setq require-final-newline t)

(setq custom-file (expand-file-name "custom-vars.el" "~/.emacs.d/"))
(load custom-file 'noerror 'nomessage)

(require 'server)
(unless (server-running-p)
  (server-start))

;; Autorevert buffers
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(setq-default
 inhibit-startup-screen t               ; Disable start-up screen
 inhibit-startup-message t              ; Disable startup message
 inhibit-startup-echo-area-message user-login-name    ; Disable initial echo message
 initial-scratch-message nil             ; Empty the initial *scratch* buffer
 initial-major-mode 'fundamental-mode)

;; Set encding by default
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL

;; Disable warnings
(setq native-comp-async-report-warnings-errors nil)

(setq auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
      (expand-file-name ".auto-save-list/.saves-" user-emacs-directory)
      auto-save-default t        ; Auto-save every buffer that visits a file
      auto-save-timeout 20       ; Number of seconds between auto-save
      auto-save-interval 200)    ; Number of keystrokes between auto-saves

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(use-package savehist
 :straight nil
 :config
(setq history-length 25)
(savehist-mode 1))

(setq kill-ring-max 25
      history-length 25)

(setq savehist-additional-variables
      '(kill-ring
        command-history
        set-variable-value-history
        custom-variable-history
        query-replace-history
        read-expression-history
        minibuffer-history
        read-char-history
        face-name-history
        bookmark-history
        file-name-history))

 (put 'minibuffer-history         'history-length 25)
 (put 'file-name-history          'history-length 25)
 (put 'set-variable-value-history 'history-length 25)
 (put 'custom-variable-history    'history-length 25)
 (put 'query-replace-history      'history-length 25)
 (put 'read-expression-history    'history-length 25)
 (put 'read-char-history          'history-length 25)
 (put 'face-name-history          'history-length 25)
 (put 'bookmark-history           'history-length 25)


;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

(setq history-delete-duplicates t)
(let (message-log-max)
  (savehist-mode))

(setq-default use-short-answers t                     ; Replace yes/no prompts with y/n
              confirm-nonexistent-file-or-buffer nil) ; Ok to visit non existent files

(defun lem/delete-file ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
     (if (y-or-n-p (concat "Do you really want to delete file " filename "?"))
            (progn (delete-file filename)
                   (message "File delete")
                   (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun lem/sync (path)
"Call the sync comand with the project to be syncrhonize."
  (shell-command-to-string (format "/home/lucas/.local/bin/sync.sh %s" path)))

(defun lem/sync-org ()
  "Sync the Org foler with an external script."
  (interactive)
  (lem/sync "~/Documents/Org"))

(defun lem/sync-conf ()
  "Sync the config foler with an external script."
  (interactive)
  (lem/sync "~/Documents/git/dotfiles"))

(use-package paren
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
;; Set up the visible bell
(setq visible-bell t)
(electric-indent-mode -1)
(electric-pair-mode -1)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

(setq frame-title-format "%b - GNU Emacs"
      icon-title-format frame-title-format)


(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))


(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'emacs-startup-hook #'window-divider-mode)

;; UX: Favor vertical splits over horizontal ones. Monitors are trending toward
;;   wide, rather than tall.
(setq split-width-threshold 160
      split-height-threshold nil)

(setq find-file-visit-truename t
      vc-follow-symlinks t)

;;(global-display-line-numbers-mode 1)

(setq-default dispaly-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(use-package hl-line
  :straight nil
  :hook ((prog-mode . hl-line-mode)
         (text-mode . hl-line-mode)
         (conf-mode . hl-line-mode)))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; only for non gnome desktop
(defun lem/set-background (frame)
(unless (string= (getenv "DESKTOP_SESSION") "gnome")
    (set-frame-parameter (selected-frame) 'alpha '(95 . 100))
    (add-to-list 'default-frame-alist '(alpha-background . 95))))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

  (use-package minions
    :hook (doom-modeline-mode . minions-mode))

(use-package doom-themes
  :defer t
  :init 
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  ;; Sets the default theme to load!!! 
 (load-theme 'doom-dracula t))

(use-package fixed-pitch
  :straight (:type git :host github :repo "cstby/fixed-pitch-mode"))

(defvar lem-default "FiraCodeNerdFont")
  (defun lem/set-fonts (frame)
    (select-frame frame)
    (set-face-attribute 'default nil
                        :family "Monospace"
                        :width 'normal
                        :weight 'normal
                        :weight 'normal
                        :height 110)

    (set-face-attribute 'fixed-pitch nil
                        :inherit 'default
                        :family lem-default)

    (set-face-attribute 'variable-pitch nil
                        :family "Iosevka Aile"
                        :inherit 'default
                        :weight 'light
                        ))
  (add-to-list 'default-frame-alist '(family . lem-default))

(setq display-time-format "%H:%M %b %y"
      display-time-default-load-average nil)
(display-time-mode 1)
(timeclock-mode-line-display 1)
;; Dimish modeline clutter hides pesky minor modes
(use-package diminish)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-minor-modes nil
        doom-modeline-persp-name nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type 0))

(when (daemonp)
  (add-hook 'after-make-frame-functions (lambda (frame)
                                          (lem/set-fonts frame)
                                          (lem/set-background frame))))

(use-package rainbow-delimiters
  :init (setq rainbow-delimiters-max-face-count 4)
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; By default, Emacs requires you to hit ESC trhee times to escape quit the minibuffer
(global-set-key [escape] 'keyboard-escape-quit)

(use-package undo-tree
:init (global-undo-tree-mode 1)
:config
(setq undo-tree-auto-save-history nil))

;; disable the arrows in insert mode
(defun rune/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package evil
  :init
  (setq evil-want-integration t
      evil-want-keybinding nil
      evil-want-C-u-scroll t
      evil-want-C-i-jump t
      evil-undo-system 'undo-tree
      evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
     ;;; Disable arrow keys in insert mode
  (define-key evil-insert-state-map (kbd "<left>") 'rune/dont-arrow-me-bro)
  (define-key evil-insert-state-map (kbd "<right>") 'rune/dont-arrow-me-bro)
  (define-key evil-insert-state-map (kbd "<down>") 'rune/dont-arrow-me-bro)
  (define-key evil-insert-state-map (kbd "<up>") 'rune/dont-arrow-me-bro))

(use-package evil-collection
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))


(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-a -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "C-a g +") 'evil-numbers/inc-at-pt-incremental)
  (define-key evil-normal-state-map (kbd "C-a g -") 'evil-numbers/dec-at-pt-incremental))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3
        which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → " ))

(use-package general
    :straight t
    :config
    (general-evil-setup t)
      (general-create-definer lem/leader-key-def
        :keymaps '(normal insert visual emacs)
        :prefix "SPC"
        :global-prefix "C-SPC")
  ;; The general use-package is note close

(setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
(lem/leader-key-def
  "b" '(:ignore t :which-key "buffers/bookmarks")
  "bl" '(list-bookmarks :which-key "List bookmarks")
  "bm" '(bookmark-set :which-key "Set bookmark")
  "bd" '(bookmark-delete :which-key "Delete bookmark")
  "bw" '(bookmark-save :which-key "Save current bookmark to bookmark file"))

(lem/leader-key-def
  "bi" '(counsel-switch-buffer :which-key "Counsel switch buffer")
  "bk" '(kill-current-buffer :whick-key "Kill current buffer")
  "bn" '(next-buffer :whick-key "Goto next buffer")
  "bp" '(previous-buffer :whick-key "Goto previous-buffer buffer")
  "bs" '(save-buffer :whick-key "Save current buffer"))

(lem/leader-key-def
  "d" '(:ignore t :wk "Dired")
  "d d" '(dired :wk "Open dired"))

(lem/leader-key-def
  "e" '(:ignore t :wk "Eshell/Evaluate")
  "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
  "ed" '(eval-defun :wk "Evaluate defun containing or after point")
  "ee" '(eval-expression :wk "Evaluate and elisp expression")
  "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
  "er" '(eval-region :wk "Evaluate elisp in region"))

(lem/leader-key-def
  "f" '(:ignore t :which-key  "Files")
  "fd" '(find-grep-dired :whick-key "Search for string in files in DIR")
  "fr" '(counsel-recentf :which-key "Recent files")
  "fD" '(lem/delete-file :which-key "Delete current file")
  "ff" '(counsel-find-file :which-key "Find files"))

(lem/leader-key-def
  "fp" '((lambda ()
           (interactive)
           (counsel-find-file "~/Documents/git/dotfiles"))
         :which-key "Config")
  "fc" '((lambda ()
           (interactive)
           (find-file "~/Documents/git/dotfiles/emacs/config.org"))
         :which-key "Emacs Config file"))

(lem/leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(lem/leader-key-def
 "=" '(perspective-map :which-key "map"))

(lem/leader-key-def
  "p"  '(:ignore t :which-key "Projectile")
  "pf" '(projectile-find-file :which-key "Projectile find file")
  "ps" '(projectile-switch-project :which-key "Projectile switch project")
  "pF" '(counsel-projectile-rg :which-key "Rip grep")
  "pc" '(projectile-compile-project :which-key "Compile Project")
  "pd" '(projectile-dired :which-key "Projectile dired")
  "pp" '(counsel-projetile :which-key "Counsel projectile"))

(lem/leader-key-def
     "o"   '(:ignore t :which-key "org mode")
     "oi"  '(:ignore t :which-key "Insert")
     "oil" '(org-insert-link :which-key "insert link")
     "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
     "os"  '(lem/org-search :which-key "search notes")
     "oa"  '(org-agenda :which-key "Status")
;;   "ot" '(org-todo-list :which-key "Show TODOs")
     "oc" '(org-capture t :which-key "Capture")
     "oC" '(:ignore t :which-key "Org clock")
     "oCe" '(org-set-effort :which-key "Org set effort")
     "oCg" '(org-clock-goto :which-key "Go ot the last clock active")
     "oCi" '(org-clock-in :which-key "Clock in in the current task")
     "oCI" '(org-clock-in-last :which-key "Clock-in the last task")
     "oCo" '(org-clock-out :which-key "Clock-out current clock")
     "on"  '((lambda () (interactive) (counsel-find-file org-directory)) :which-key "Notes")
     "op" '(:ignore t :which-key "Pomodoro")
     "ops" '(org-pomodoro :whick-key "Start org pomodoro")
     "opt" '(set-pomodoro-timer :which-key "Set pomodoro timer")
     "ot"  '(:ignore t :which-key "Insert time stamp")
     "ots" '(org-time-stamp :which-key "Insert active time stamp")
     "oti" '(org-time-stamp-inactive :which-key "Insert inactive stamp"))

(lem/leader-key-def
  "or"  '(:ignore t :which-key "Org roam")
  "orl" '(org-roam-buffer-togle :which-key "Org roam buffer togle")
  "orf" '(org-roam-node-find :whick-key "Org roam node find")
  "ori" '(org-roam-node-insert :whick-key "Org roam node insert")
  "orI" '(org-roam-node-insert-immediate :which-key "Roam insert immediately")
  "orc" 'lem/org-roam-capture-task)

(lem/leader-key-def
"s" '(:ignore t :which-key "sync")
"so" '(lem/sync-org :which-key "Sync org files")
"sc" '(lem/sync-conf :which-key "Sync config folder"))

(lem/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" '(whitespace-mode :which-key "whitespace")
  "td" '(lem/switch-dictionary :which-key "Toggle between dictionaries")
  "tl" '(org-toggle-link-display :which-key "Toggle org link display"))

(lem/leader-key-def
   "u" '(universal-argument :which-key "Universal argument"))

;; end of general parents
)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(use-package perspective
    :custom
    (persp-mode-prefix-key (kbd "C-x x"))
    :init (persp-mode)
    :config
    (setq persp-state-default-file (expand-file-name "sessions" user-emacs-directory)))
  ;; Use ibuffer with perspective

  (add-hook
   'ibuffer-hook (lambda ()
                   (persp-ibuffer-set-filter-groups)
                   (unless (eq ibuffer-sorting-mode 'alphabetic)
                     (ibuffer-do-sort-by-alphabetic))))

;; Automatically save perspective states to file when Emacs exits.
(add-hook 'kill-emacs-hook #'persp-state-save)

(use-package company
  :defer t
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto nil) ;; if active make it to slow
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package hydra
  :defer 1)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                           ;; Don't mess with EXWM buffers
                           (with-current-buffer buffer
                             (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         ;; ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function
   #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(setq ivy-initial-inputs-alist nil)

(use-package smex
:config
(smex-initialize))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :config
  (setq dired-listing-swithces "--group-directories-first"
        dired-omit-files "^\\.[^.].*"
        delete-by-moving-to-trash t)
  (autoload 'dired-omit-mode "dired-x")
  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))
  (add-hook 'dired-mode-hook
            (lambda () (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (all-the-icons-dired-mode 1)
              (hl-line-mode 1))))

(use-package dired-single :defer t)
(use-package dired-ranger :defer t)
(use-package dired-collapse :defer t)

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "H" 'dired-omit-mode
  "l" 'dired-single-buffer
  "y" 'dired-ranger-copy
  "X" 'dired-ranger-move
  "p" 'dired-ranger-paste)

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package flyspell
    :config
    (setq ispell-program-name "hunspell"
    ispell-default-dictionary "en_US")
    :hook (text-mode . flyspell-mode)
    :bind (("M-<f7>" . flyspell-buffer)
     ("<f7>" . flyspell-word)
     ("C-;" . flyspell-auto-correct-previous-word)))

(defun lem/switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
        (change (if (string= dic "en_US") "es_ES" "en_US")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(use-package langtool
  :config
  (setq langtool-language-tool-jar
        "~/.local/lib/languageTool/LanguageTool-6.3/languagetool-commandline.jar"
        langtool-default-language "en-US"))

;; Wrap the text in a custom column size
(use-package visual-fill-column)

(defun lem/text-mode-setup ()
  (setq fill-column 80)
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  ;(visual-fill-column-mode 1)
  (setq evil-auto-indent nil))

(add-hook 'text-mode-hook 'lem/text-mode-setup)

(setq-default fill-column 80                          ; Default line width
              sentence-end-double-space nil           ; Use a single space after dots
              bidi-paragraph-direction 'left-to-right ; Faster
              truncate-string-ellipsis "…")

(use-package markdown-mode
    :straight t
    :mode "\\.mdx?\\'"
    :config
  (setq markdown-command "marked"))

;;  (custom-set-faces '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch")))))
;;  (dolist (face '((markdown-header-face-1 . 1.3)
;;                  (markdown-header-face-2 . 1.25)
;;                  (markdown-header-face-3 . 1.2)
;;                  (markdown-header-face-4 . 1.15)
;;                  (markdown-header-face-5 . 1.1)
;;                  (markdown-header-face-6 . 1.05)))
;;    (set-face-attribute (car face) nil :height (cdr face)))

(use-package projectile
  :init
  (setq projectile-auto-discover nil
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-kill-buffers-filter 'kill-only-files)
  :diminish projectile-mode
  :config (projectile-mode +1)
  :demand t)

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(use-package flycheck
  :straight t
  :defer t
  :diminish
  :init (global-flycheck-mode))

(if (version< emacs-version "29.0")
  (use-package seq))
(use-package magit)

(use-package git-gutter
  :straight t
  :diminish
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 0.2))

(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(defun dt-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'dt-ediff-hook)

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 10000))

(add-to-list 'display-buffer-alist
             '("\*vterm\*"
               (display-buffer-in-side-window)
               (window-height . 0.25)
               (side . bottom)
               (slot . 0)))
;;Still does not work
(add-hook 'vterm-mode-hook 'evil-emacs-state)
(add-hook 'term-mode-hook 'evil-emacs-state)

(use-package vterm-toggle
  :after vterm
  :config
  ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
  ;; kills the program as it would in most standard terminal programs.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.4))))

(use-package multi-vterm)

(use-package sudo-edit)

(use-package compile
  :straight nil
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
                                        ; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil))

(use-package prettier-js
  :config
  (setq prettier-js-show-errors nil))

(use-package web-mode)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  (defun lem/custom-web-mode ()
    "Hook to setting the web-mode."
    (setq web-mode-markup-indent-offset 2 ;; for html
          web-mode-css-indent-offset    2 ;; for css
          web-mode-code-indent-offset   4 ;; for script/code
          web-mode-enable-auto-pairing  t
          web-mode-style-padding        4
          web-mode-script-padding       4))
(add-hook 'web-mode-hook 'lem/custom-web-mode)

(use-package toc-org
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(defun lem/org-mode-hook ()
  ;(org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode 0)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . lem/org-mode-hook)
  :config
  (setq org-directory "~/Documents/Org/"
        org-default-notes-file (concat org-directory "Inbox.org")
        org-log-done 'time
        org-hide-emphasis-markers t
        org-table-convert-region-max-lines 20000
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-cycle-separator-lines 2
        org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 1))
        org-outline-path-complete-in-steps nil
        org-refile-use-outline-path t
        org-latex-create-formula-image-program 'dvisvgm
        org-link-frame-setup '((file . find-file)) ;; open file in the same window
        org-startup-folded 'showall ;; when emacs set as default the value showeverithing, overwrite custom visibilities
        )

(setq 
 org-indirect-buffer-display 'current-window
 org-enforce-todo-dependencies t
 org-fontify-done-headline t
 org-fontify-quote-and-verse-blocks t
 org-fontify-whole-heading-line t
 org-tags-columns 0
 )

(with-no-warnings
  (custom-declare-face 'org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face 'org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")

  (custom-declare-face 'org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
  (custom-declare-face 'org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "STRT(s)"
         "WAIT(w)"
         "HOLD(h)"
         "|"
         "DONE(d!)"
         "CANCELED(c!)")
        (sequence
         "[ ](T)"
         "[-](S)"
         "[?](W)"
         "|"
         "[X](D)"))
      org-todo-keywords-faces
      '(("[-]" . org-todo-active)
        ("STRT" . org-todo-active)
        ("[?]" . org-todo-onhold)
        ("WAIT" . org-todo-onhold)
        ("HOLD" . org-todo-onhold)))

(defun lem/start-task () 
  "Start a clock when a task change the state from TOOD to IN PROGRESS."
    (when (string= (org-get-todo-state) "STRT")
           (org-clock-in))
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'lem/start-task)

(setq org-tag-alist
    '((:startgroup . nil)
     ;Put mutually exclusive tags here
     ("@home" . ?H )
     ("@PHD" . ?P)
     ("@UI" . ?U)
     (:endgroup . nil)))

(setq org-agenda-files
      (mapcar (lambda (file)
                (concat org-directory file)) '("Tasks.org" "Habits.org" "Projects.org"))
      org-agenda-window-setup 'current-window
      org-agenda-span 'week
      org-agenda-start-with-log-mode t
      org-agenda-time-in-grid t
      org-agenda-show-current-time-in-grid t
      org-agenda-start-on-weekday 1
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-log-into-drawer t
      org-columns-default-format "%20CATEGORY(Category) %30ITEM(Task) %4TODO %6Effort(Estim){:} %20SCHEDULED %20DEADLINE %6CLOCKSUM(Clock) %TAGS")

(setq org-clock-persist t)
(org-clock-persistence-insinuate)

(setq org-capture-templates
      `(("t" "Task" entry
         (file+headline ,(concat org-directory "Tasks.org") "Inbox")
         "* TODO %?\nAdded at: %U" :empty-lines 1)))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60
      org-habit-show-all-today nil
      org-habit-show-habits-only-for-today nil)

(setq org-agenda-custom-commands
      `(("d" "Dashboard" 
         ((agenda ""
                  ((org-deadline-warning-days 7)
                   (org-agenda-span 'day)
                   (org-agenda-overriding-header "Agenda")
                   ))
          (alltodo ""
                   ((org-agenda-overriding-header "Sort by priority")
                    (org-agenda-sorting-strategy '(priority-down)))
                   (org-agenda-todo-ignore-scheduled 'all)
                   (org-agenda-todo-ignore-scheduled 'all)
                   org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))
         (todo "ACTIVATED"
               ((org-agenda-overriding-header "Next Actions")
                (org-agenda-max-todos nil)))
         (todo "TODO"
               ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                (org-agenda-files '(,(concat org-directory "Tasks.org")))
                (org-agenda-text-search-extra-files nil)))
         )))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (shell . t)
   (gnuplot . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))
(add-to-list 'org-structure-template-alist '("ex" . "export"))

(use-package org-superstar
  :after org
  :hook (org-mode . (lambda () (org-superstar-mode 1)))
  :config
(setq org-superstar-remove-leading-stars t
      org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-indent)
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-date nil :inherit 'fixed-pitch)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil
                    :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil
                    :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(setq org-capture-templates
      (append org-capture-templates
              `(("m" "Fondos" table-line
                 (file+headline
                  ,(expand-file-name "Metrics.org" org-directory) "Fondos")
                 "| %U | %^{fondo1} | %^{fondo2} |" :kill-buffer t)
                ("j" "Journal" entry
                 (file+olp+datetree
                  ,(expand-file-name "Journal.org" org-directory) "Notes")
                  "\n* %<%H:%m>\nFrom: %a\n%?" :empty-lines 1)
                )))

)

(use-package org-pomodoro
:config
(setq
  alert-user-configuration
  (quote ((((:category . "org-pomodoro")) libnotify nil)))
 org-pomodoro-length 90
 org-pomodoro-short-break-length 10
 org-pomodoro-long-break-length 20
 org-pomodoro-clock-break t
 org-pomodoro-manual-break t))

(defun set-pomodoro-timer (minutes)
  (interactive "nMinutes: ")
  (setq org-pomodoro-length minutes))

;; function to search into the org folder
(defun lem/org-search ()
  (interactive)
  (counsel-rg "" org-directory nil "Search notes: "))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default nil))
(defun lem/insert-auto-tangle-tag ()
  "Insert auto-tangle tag in literature config."
  (interactive)
  (evil-org-open-below 1)
  (insert "#+auto_tangle: t ")
  (evil-force-normal-state))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
        (org-agenda-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :demand t
  :straight t
  :config
  (setq org-roam-directory (expand-file-name "roam" org-directory)
      org-roam-db-autosync-mode t))

(setq org-roam-capture-templates
 '(("f" "Fleeting" plain "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n#+AUTHOR: %n\n#+filetags: fleeting")
     :unnarrowed nil)
   ("d" "default" plain "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+author: %n\n")
    :unnarrowed t)
   ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
    :unnarrowed t)))

(setq org-roam-node-display-template
      (concat "${title:*} "
              (propertize "${tags:30}" 'face 'org-tag)))

(defun lem/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun lem/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (lem/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))
  (defun lem/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (delete-dups (append org-agenda-files (lem/org-roam-list-notes-by-tag "Project")))))

  (defun lem/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'lem/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (lambda (node)
       (member "Project" (org-roam-node-tags node)))))
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))
  (defun lem/org-roam-capture-task ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-read
                              nil
                              (lem/org-roam-filter-by-tag "Project"))
                       :templates '(
                                    ("p" "project" plain "** TODO %?"
                                     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                            "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                            ("Tasks")))
                                    ("s" "start now" entry "** TODO %?"
                                     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                            "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                            ("Tasks"))
                                     :clock-in :clock-resume)
                                    ("m" "Meeting")
                                    ("mp" "Prepare meeting" entry "** Notes\n %?"
                                     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                            "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                            ("Meetings"))
                                     :target (file+olp+datetree "%<%Y%m%d%H%M%S>-${slug}.org" ("Meetings"))))))

; initialize the functions
(lem/org-roam-refresh-agenda-list)

(setq lem/bibliography-files '("~/Documents/Org/bibliography.bib" "~/Documents/Org/phd.bib"))
(use-package ivy-bibtex
  :config
  (setq bibtex-completion-additional-search-fields '(keywords abstract)
        bibtex-completion-bibliography lem/bibliography-files
        bibtex-completion-format-citation-functions
            '((org-mode . bibtex-completion-format-citation-org-title-link-to-PDF)
  	      (latex-mode . bibtex-completion-format-citation-cite)
  	      (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
  	      (python-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
  	      (rst-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
  	      (default . bibtex-completion-format-citation-default))))

(use-package citar)
(use-package citar-org-roam
  :after org-roam citar
  :config
  (setq citar-bibliography lem/bibliography-files
        org-cite-global-bibliography lem/bibliography-files
        citar-notes-paths org-roam-directory
        citar-notes-source 'orb-citar-source
        citar-org-roam-note-title-template "${author} - ${title}")

  :custom
  (citar-register-notes-source 'orb-citar-source
                               (list :name "Org-Roam Notes"
                                     :category 'org-roam-node
                                     :items #'citar-org-roam--get-candidates
                                     :hasitems #'citar-org-roam-has-notes
                                     :open #'citar-org-roam-open-note
                                     :create #'orb-citar-edit-note
                                     :annotate #'citar-org-roam--annotate)))

(use-package org-roam-bibtex
  :after org-roam
  :init (org-roam-bibtex-mode 1)
  :config
  (setq org-inser-interface 'ivy-bibtex
      orb-note-actions-interface 'ivy
      orb-preformat-keywords '("citekey" "author" "date" "entry-type" "keywords" "url" "file")
      citar-notes-source 'orb-citar-source)

(add-to-list 'org-roam-capture-templates
               '("r" "bibliography reference" plain "%?"
                 :if-new (file+head "%<%Y%m%d%H%M%S>-${citekey}.org"
                                    "#+TITLE: ${title}\n#+AUTHOR: ${author}\n#+filetags: Literature\n#+cite-key: ${citekey}\n#+cite-date: ${date} \n#+created: %U\n\n* ${title}\n\n")
                 :unnarrowed t) t))

(defun lem/import-notes-from-zotero (citekey)
(interactive "sCiteKey: ")
  (let* ((entry (bibtex-completion-get-entry citekey))
         (note (bibtex-completion-get-value "note" entry ""))
         (pandoc-command "pandoc --from latex --to org")
         result)
    (with-temp-buffer
      (shell-command (format "echo \"%s\" | %s" note pandoc-command)
                     (current-buffer))
      (setq result (buffer-substring-no-properties (point-min) (point-max))))
    (insert result)))

(defun lem/add-acronym (label abbrv full)
  (interactive "sLabel: \nsAccronym: \nsFull text: ")
  (save-excursion
    (re-search-backward "#\\+latex_header" nil t)
    (forward-line)
    (when (not (looking-at "^$"))
      (beginning-of-line)
      (insert "\n")
      (forward-line -1))
    (insert (format "#+latex_header_extra: \\newacronym{%s}{%s}{%s}\n"
                    label abbrv full))
  (write-region
     (format
      "\\newacronym{%s}{%s}{%s}\n"
      label abbrv full)
     nil "~/Documents/Org/roam/glossary.tex" 'append)))

(use-package org-roam-ui)
