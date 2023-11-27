;; -*- lexical-binding: t; -*-
;(server-start)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Clean up unused repos with `straight-remove-unused-repos'

(use-package gcmh
:config
(gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold (* 50 1000 1000)
    gc-cons-percentage 0.6)
;; Profile emacs startup
(add-hook 'emacs-startup-hook
        (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                    (format "%.2f seconds"
                            (float-time
                            (time-subtract after-init-time before-init-time)))
                    gcs-done)))

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
;;
;; Use no-littering to automatically set common paths to the new user-emacs-directory

(use-package no-littering)

(setq backup-directory-alist '(("." . "~/.cache/emacs/backup/"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(push "~/Documents/git/dotfiles/emacs.d/lisp" load-path)

(set-default-coding-systems 'utf-8)

(setq native-comp-async-report-warnings-errors nil)

(server-start)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "C-M-u") 'universal-argument)

;; Define a hook to prevent evil mode be load in some modes
(defun lem/evil-custom-state ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

(defun rune/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
:config
(setq undo-tree-auto-save-history nil))

(use-package evil
 :init
 (setq evil-want-integration t)
 (setq evil-want-keybinding nil)
 (setq evil-want-C-u-scroll t)
 (setq evil-want-C-i-jump t)
 (setq evil-undo-system 'undo-tree)
 :config
 (add-hook 'evil-mode-hook 'lem/evil-custom-state)
 (evil-mode 1)
 (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
 (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
 (evil-set-initial-state 'messages-buffer-mode 'normal)
 (evil-set-initial-state 'dashboard-mode 'normal)
(evil-set-initial-state 'term-mode 'emacs)

  ;;; Disable arrow keys in insert mode
  (define-key evil-insert-state-map (kbd "<left>") 'rune/dont-arrow-me-bro)
  (define-key evil-insert-state-map (kbd "<right>") 'rune/dont-arrow-me-bro)
  (define-key evil-insert-state-map (kbd "<down>") 'rune/dont-arrow-me-bro)
  (define-key evil-insert-state-map (kbd "<up>") 'rune/dont-arrow-me-bro))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))


(use-package evil-numbers
  :after evil
  :hook 'lem/evil-mode-number-hook)

(define-key evil-normal-state-map (kbd "C-a +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-a -") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-a g +") 'evil-numbers/inc-at-pt-incremental)
(define-key  evil-normal-state-map (kbd "C-a g -") 'evil-numbers/dec-at-pt-incremental)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer lem/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(setq inhibit-startup-message t)

  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room

  (menu-bar-mode -1)            ; Disable the menu bar

  ;; Set up the visible bell
  (setq visible-bell t)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha-background . 90))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

(setq vc-follow-symlinks t)

(use-package spacegray-theme :defer t)
(use-package doom-themes :defer t)
(load-theme 'doom-dracula t)
(doom-themes-visual-bell-config)

;; Set the font
(setq default-mono-font "Fira Code"
          default-variable-pitch-font "Noto Sans")
  (set-face-attribute 'default nil :font default-mono-font :height 120)
  (set-face-attribute 'fixed-pitch nil :family default-mono-font :height 1.0 :inherit 'default)
  (set-face-attribute 'variable-pitch nil :family default-variable-pitch-font :weight 'regular :inherit 'default)

(setq display-time-format "%H:%M %b %y"
    display-time-default-load-average nil)
(display-time-mode 1)
  ;; Dimish modeline clutter hides pesky minor modes
  (use-package diminish)

  ;; All the icons
  (use-package all-the-icons)
  (use-package minions
    :hook (doom-modeline-mode . minions-mode))

  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom
    (doom-modeline-height 15))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(lem/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" '(whitespace-mode :which-key "whitespace"))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "ssh"))

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)
;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(lem/leader-key-def
"fd" '((lambda () (interactive) (counsel-find-file "~/Documents/git/dotfiles")) :which-key "Config")
"fo" '((lambda () (interactive) (counsel-find-file "~/Documents/Org/")) :which-key "Org files")
"fe" '(:ignore t :which-key "Emacs Config")
"fec" '((lambda () (interactive) (find-file "~/Documents/git/dotfiles/.emacs.d/config.org")) :which-key "Emacs Config file")
"few" '((lambda () (interactive) (find-file "~/Documents/git/dotfiles/.emacs.d/org-workflow.org")) :which-key "Emacs workflow file")
"fez" '((lambda () (interactive) (find-file "~/Documents/git/dotfiles/.emacs.d/org-zettelkasten.org")) :which-key "Emacs zettel file"))

(setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
(lem/leader-key-def
"b" '(:ignore t :which-key "buffers/bookmarks")
"bL" '(list-bookmarks :which-key "List bookmarks")
"bm" '(bookmark-set :which-key "Set bookmark")
"bd" '(bookmark-delete :which-key "Delete bookmark")
"bw" '(bookmark-save :which-key "Save current bookmark to bookmark file"))

(lem/leader-key-def
"bi" '(counsel-switch-buffer :which-key "Counsel switch buffer")
"bk" '(kill-current-buffer :whick-key "Kill current buffer")
"bn" '(next-buffer :whick-key "Goto next buffer")
"bp" '(previous-buffer :whick-key "Goto previous-buffer buffer")
"bs" '(save-buffer :whick-key "Save current buffer"))

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

(lem/leader-key-def
  "C-S" '(counsel-projectile-grep :which-key "Projectile grep"))


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
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(setq ivy-initial-inputs-alist nil)

(use-package smex)
(smex-initialize)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company
:after lsp-mode
:hook (prog-mode . company-mode)
:bind (:map company-active-map
            ("<tab>" . company-complete-selection))
(:map lsp-mode-map
      ("<tab>" . company-indent-or-complete-common))
:custom
(company-minimum-prefix-length 1)
(company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package all-the-icons-dired)
(use-package dired
  :ensure nil
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
              (hl-line-mode 1)))

(use-package dired-single
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package dired-collapse
  :defer t)

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "H" 'dired-omit-mode
  "l" 'dired-single-buffer
  "y" 'dired-ranger-copy
  "X" 'dired-ranger-move
  "p" 'dired-ranger-paste))

(defun lem/delete-file ()
  "Delete the current file and kill the buffer"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename "?"))
            (progn (delete-file filename)
                   (message "File delete")
                   (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(lem/leader-key-def
  "f" '(:ignore t :which-key  "Files")
  "fr" '(counsel-recentf :which-key "Recent files")
  "fD" '(lem/delete-file :which-key "Delete current file")
  "ff" '(counsel-find-file :which-key "Find files"))

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

(global-set-key (kbd "<f8>")   'lem/switch-dictionary)

(use-package langtool
   :config
   (setq langtool-language-tool-jar "~/.local/lib/languageTool/LanguageTool-6.3/languagetool-commandline.jar"
langtool-default-language "en-US"))

;; Wrap the text in a custom column size
(defun lem/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        fill-column 80
        visual-fill-column-center-text t))

(use-package visual-fill-column
  :defer t
  :hook (text-mode . lem/org-mode-visual-fill))

(defun lem/sync (path)
    (shell-command-to-string (format "/home/lucas/.local/bin/sync.sh %s" path)))

  (defun lem/sync-org ()
  "Sync the Org foler with an external script"
  (interactive)
  (lem/sync "~/Documents/Org"))

  (defun lem/sync-conf ()
  "Sync the config foler with an external script"
(interactive)
(lem/sync "~/Documents/git/dotfiles"))

  ;; (add-hook 'after-save-hook 'lem/sync) Use as hook generate a lot of commits

(defun lem/text-mode-setup ()
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  (visual-fill-column-mode 1)
  (setq evil-auto-indent nil))

(add-hook 'text-mode-hook 'lem/text-mode-setup)

(defun lem/org-mode-hook ()
  (org-indent-mode)
    (diminish org-indent-mode))

(use-package org
:hook (org-mode . lem/org-mode-hook)
:config
(setq org-directory "~/Documents/Org/"
      org-default-notes-file (concat org-directory "Inbox.org")
      org-ellipsis " ▾"
      org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
      org-superstar-item-bullet-alist '((?- . ?➤) (?+ . ?✦)) ; changes +/- symbols in item lists
      org-log-done 'time
      org-hide-emphasis-markers nil
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
      org-refile-use-outline-path t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c!)")))

(setq org-tag-alist
    '((:startgroup . nil)
     ;Put mutually exclusive tags here
     ("@home" . ?H )
     ("@PHD" . ?P)
     ("@UI" . ?U)
     (:endgroup . nil)))

(setq org-fancy-priorities-list '("🟥" "🟧" "🟨")
      org-priority-faces
      '((?A :foreground "#ff6c6b" :weight bold)
        (?B :foreground "#98be65" :weight bold)
        (?C :foreground "#c678dd" :weight bold))
      org-agenda-block-separator 8411)

(setq org-agenda-files
      (mapcar (lambda (file) (concat org-directory file)) '("Tasks.org" "Habits.org"))
      org-agenda-window-setup 'current-window
      org-agenda-span 'week
      org-agenda-start-with-log-mode t
      org-log-into-drawer t
      org-columns-default-format "%20CATEGORY(Category) %30ITEM(Task) %4TODO %6Effort(Estim){:} %16SCHEDULED %6CLOCKSUM(Clock) %TAGS")

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-capture-templates
      `(("t" "Tasks")
        ("tt" "Task" entry (file+olp+datetree ,(concat org-directory "Tasks.org"))
         "* TODO %?\n  %i"
         :empty-lines 1)))

(use-package org-pomodoro
    :ensure t
    :after org
    :config
    (setq
     alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
     org-pomodoro-length 90
     org-pomodoro-short-break-length 10
     org-pomodoro-long-break-length 20
     org-pomodoro-clock-break t
     org-pomodoro-manual-break t))

  (defun set-pomodoro-timer (minutes)
    (interactive "nMinutes: ")
    (setq org-pomodoro-length minutes))

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60
      org-habit-show-all-today nil
      org-habit-show-habits-only-for-today nil)

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
(add-to-list 'org-structure-template-alist '("js" . "src python"))
(add-to-list 'org-structure-template-alist '("ex" . "export"))

(use-package org-superstar
:after org
:hook (org-mode . org-superstar-mode))

(require 'org-indent)
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-date nil :inherit 'fixed-pitch)
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; function to search into the org folder
(defun lem/org-search ()
  (interactive)
  (counsel-rg "" org-directory nil "Search notes: "))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(lem/leader-key-def
  "o" '(:ignore t :which-key "org mode")
  "oi" '(:ignore t :which-key "Insert")
  "oil" '(org-insert-link :which-key "insert link")
  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")
  "os"  '(lem/org-search :which-key "search notes")
  "oa" '(org-agenda :which-key "Status")
  ;;"ot" '(org-todo-list :which-key "Show TODOs")
  "oc" '(org-capture t :which-key "Capture")
  "op" '(:ignore t :which-key "Pomodoro")
  "ops" '(org-pomodoro :whick-key "Start org pomodoro")
  "opt" '(set-pomodoro-timer :which-key "Set pomodoro timer"))

)

(add-to-list 'org-capture-templates
`("m" "Fondos" table-line                                        
                 (file+headline ,(expand-file-name "Metrics.org" org-directory) "Fondos")
                 "| %U | %^{fondo1} | %^{fondo2} |" :kill-buffer t) t)

(use-package toc-org                                                          
  :hook (org-mode . toc-org-mode))

(use-package org-auto-tangle
    :defer t
    :hook (org-mode . org-auto-tangle-mode)
    :config
    (setq org-auto-tangle-default t))

  (defun lem/insert-auto-tangle-tag ()
    "Insert auto-tangle tag in literature config."
    (interactive)
    (evil-org-open-below 1)
    (insert "#+auto_tangle: t ")
    (evil-force-normal-state))

(lem/leader-key-def
  "ia" '(lem/insert-auto-tangle-tag :which-key "Insert auto-tangle header tag"))

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n\n* Time Managment\n#+BEGIN: clocktable :scope agenda :maxlevel 6 :block %<%Y-%m-%d>\n#+CAPTION: \n#+END:" ("Notes")))))
  :bind (("C-c n l" . org-roam-buffer-togle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  ;;Autosync mode allows to keep track and cache all changes to maintain cache consistency. Also this configuration parameter was moved to the package declaration
  (org-roam-db-autosync-mode)
  ;; refresh agenda list after load org-roam
  (my/org-roam-refresh-agenda-list)

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
              (propertize "${tags:*}" 'face 'org-tag)))

)

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))
(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (delete-dups (append org-agenda-files (my/org-roam-list-notes-by-tag "Project")))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (lambda (node)
     (member "Project" (org-roam-node-tags node)))))

(defun my/org-roam-capture-task ()
(interactive)
(org-roam-capture- :node (org-roam-node-read
                              nil
                              (my/org-roam-filter-by-tag "Project"))
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
                                  ("mp" "Prepare meeting" entry "**** Notes\n %?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Meetings"))
                                   :target (file+olp+datetree "%<%Y%m%d%H%M%S>-${slug}.org" ("Meetings"))))))

(lem/leader-key-def
  "or"  '(:ignore t :which-key "Org roam")
  "orl" '(org-roam-buffer-togle :which-key "Org roam buffer togle")
  "orf" '(org-roam-node-find :whick-key "Org roam node find")
  "ori" '(org-roam-node-insert :whick-key "Org roam node insert")
  "orI" '(org-roam-node-insert-immediate :which-key "Org roam insert immediately")
  "orc" 'my/org-roam-capture-task)

(setq bibliography-files '("~/Documents/Org/bibliography.bib"
                             "~/Documents/Org/phd.bib"))
(use-package ivy-bibtex
  :config
  (setq bibtex-completion-bibliography bibliography-files))

  (use-package org-ref                                                                                                                                                                                             
    :init (require 'bibtex)                                                     
    :config (setq bibtex-autokey-year-length 4                                  
                  bibtex-autokey-name-year-separator "-"                        
                  bibtex-autokey-year-title-separator "-"                       
                  bibtex-autokey-titleword-separator "-"                        
                  bibtex-autokey-titlewords 2                                   
                  bibtex-autokey-titlewords-stretch 1                           
                  bibtex-autokey-titleword-length 5                             
                  org-ref-glsentries '("~/Documents/Org/roam/glossary.tex"))    
    (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)         
    (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)                
    (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)       
    (require 'org-ref-ivy)                                                      
    (require 'org-ref-sci-id)                                                   
    (require 'org-ref-arxiv)                                                    
    (require 'org-ref-scopus)                                                   
    (require 'org-ref-pubmed)                                                   
    (require 'org-ref-wos)                                                      
    (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body    
                org-ref-insert-cite-function 'org-ref-cite-insert-ivy           
                org-ref-insert-label-function 'org-ref-insert-label-link        
                org-ref-insert-ref-function 'org-ref-insert-ref-link            
                org-ref-cite-onclick-function (lambda (_)                       
                  (org-ref-citation-hydra/body))))

(use-package citar                                                            
  :custom                                                                     
  (citar-bibliography bibliography-files))

(use-package org-roam-bibtex :after org-roam)                                 
(use-package citar-org-roam                                                   
  :after (citar org-roam)                                                      
  :config                                                                      
  (citar-org-roam-mode)                                                        
  (citar-register-notes-source 'orb-citar-source                               
                               (list :name "Org-Roam Notes"                                                
                                     :category 'org-roam-node                                                   
                                     :items #'citar-org-roam--get-candidates                                    
                                     :hasitems #'citar-org-roam-has-notes                                       
                                     :open #'citar-org-roam-open-note                                           
                                     :create #'orb-citar-edit-note                                              
                                     :annotate #'citar-org-roam--annotate))                                     
  (setq citar-notes-source 'orb-citar-source)                                 

(setq citar-org-roam-note-title-template "${author} - ${title}")              
(add-to-list 'org-roam-capture-templates                                      
             '("r" "bibliography reference" plain "%?"                        
               :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"                                                                                                                                                  
                                  "#+TITLE: ${title}\n#+AUTHOR: ${author}\n#+filetags: Literature\n#+cite-key: ${citekey}\n#+cite-date: ${date} \n#+created: %U\n\n* ${title}\n\n")
               :unnarrowed t) t)                                              
(setq citar-org-roam-capture-template-key "r"))

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

(use-package dashboard
    :ensure t
    :init      ;; tweak dashboard config before loading it
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
    ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
    ;;(setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
    (setq dashboard-center-content nil) ;; set to 't' for centered content
    (setq dashboard-icon-type 'all-the-icons)
    (setq dashboard-items '((recents . 5)
                (agenda . 5 )
                (bookmarks . 3)
                (projects . 5)
                (registers . 3)))
    :config
    (dashboard-setup-startup-hook)
    (dashboard-modify-heading-icons '((recents . "file-text")
                      (bookmarks . "book"))))
  ; ensure emacs open in dashboard
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package vterm
    :commands vterm
    :config
    (setq vterm-max-scrollback 10000))

;;Still does not work
  (add-hook 'vterm-mode-hook 'evil-emacs-state)
  (add-hook 'term-mode-hook 'evil-emacs-state)

(use-package projectile
  :diminish projectile-mode
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config (projectile-mode)
  (projectile-register-project-type 'npm '("package.json")
                                    :project-file "package.json"
                                    :compile "npm install"
                                    :test "npm test"
                                    :run "npm start"
                                    :test-suffix ".spec")
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(lem/leader-key-def
  "p"  '(:ignore t :which-key "Projectile")
  "pf" '(projectile-find-file :which-key "Projectile find file")
  "ps" '(projectile-switch-project :which-key "Projectile switch project")
  "pF" '(counsel-projectile-rg :which-key "Rip grep")
  "pc" '(projectile-compile-project :which-key "Compile Project")
  "pd" '(projectile-dired :which-key "Projectile dired")
  "pp" '(counsel-projetile :which-key "Counsel projectile"))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos :defer t)

(use-package git-gutter
  :ensure t
  :diminish
  :hook ((prog-mode . git-gutter-mode)
         (text-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 0.2))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package copilot
    :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
    :ensure t)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(use-package lsp-mode
  :straight t
  :commands lsp
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable t))

(lem/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

(use-package typescript-mode
    :mode "\\.ts\\'"
    :config
    (setq typescript-indent-level 4))

  (defun lem/set-js-indentation ()
    (setq js-indent-level 4)
    (setq evil-shift-width js-indent-level)
    (setq-default tab-width 4))

  (use-package js2-mode
    :mode "\\.jsx?\\'"
    :config
    ;; Use js2-mode for Node scripts
    (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

    ;; Don't use built-in syntax checking
    (setq js2-mode-show-strict-warnings nil)

    ;; Set up proper indentation in JavaScript and JSON files
    (add-hook 'js2-mode-hook #'lem/set-js-indentation))

;;  (use-package apheleia
;;    :config
;;    (apheleia-global-mode +1))
;;
;;  (use-package prettier-js
;;    :config
;;    (setq prettier-js-show-errors nil))

(use-package web-mode
      :config
      (setq-default web-mode-code-indent-offset 2)
      (setq-default web-mode-markup-indent-offset 2)
      (setq-default web-mode-attribute-indent-offset 2))

    (use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))
    (use-package smartparens :hook (prog-mode . smartparens-mode))

  (defun lem/html-as-prog ()
    (interactive)
              (variable-pitch-mode 0)
              (auto-fill-mode 0)
              (visual-fill-column-mode 0))
(add-hook 'html-mode-hook 'lem/html-as-prog)

(use-package rainbow-mode
  :defer t
  :hook (web-mode))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode :straight t)

(use-package compile
  :straight nil
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))
