(server-start)

(use-package which-key
    :init
    (setq which-key-side-window-location 'bottom
          which-key-sort-order #'which-key-key-order-alpha
          which-key-sort-uppercase-first nil
          which-key-add-column-padding 1
          which-key-max-display-columns nil
          which-key-min-display-lines 6
          which-key-side-window-slot -10
          which-key-side-window-max-height 0.25
          which-key-idle-delay 0.8
          which-key-max-description-length 25
          which-key-allow-imprecise-window-fit t
          which-key-separator " → " ))
(which-key-mode)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(unless package-archive-contents
  (package-refresh-contents))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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

(use-package general
    :config
    (general-evil-setup t))

(use-package gcmh
:config
(gcmh-mode 1))
;; Setting garbage collection threshold
(setq gc-cons-threshold 402653184
    gc-cons-percentage 0.6)

;; Profile emacs startup
(add-hook 'emacs-startup-hook
        (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                    (format "%.2f seconds"
                            (float-time
                            (time-subtract after-init-time before-init-time)))
                    gcs-done)))

(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
(message "Emacs loaded in %s with %d garbage collections."
        (format "%.2f seconds"
                (float-time
                    (time-subtract after-init-time before-init-time)))
        gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Ivy is a completion mechanism for Emacs. Ivy-rich allows you to add descriptions alongside the command M-x

(use-package counsel
    :after ivy
    :config (counsel-mode))
(use-package ivy
    :defer 0.1
    :diminish
    :bind
    (("C-c C-r" . ivy-resume)
    ("C-x B" . ivy-switch-buffer-other-window))
    :custom
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    :config
    (ivy-mode))
(use-package ivy-rich
    :after ivy
    :custom
    (ivy-virtual-abbreviate 'full
    ivy-rich-switch-buffer-align-virtual-buffer t
    ivy-rich-path-style 'abbrev)
    :config
    (ivy-set-display-transformer 'ivy-switch-buffer
                                'ivy-rich-switch-buffer-transformer)
    (ivy-rich-mode 1)) ;; this gets us descriptions in M-x.
(use-package swiper
    :after ivy
    :bind (("C-s" . swiper)
            ("C-r" . swiper)))
(setq ivi-initial-inputs-alist nil)

(use-package smex)
(smex-initialize)

(use-package flycheck)
(flycheck-mode)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el"
                   :files ("dist" "copilot.el"))
  :ensure t)

(add-hook 'prog-mode-hook 'copilot-mode)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
  
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

(use-package ivy-posframe
  :init
  (setq ivy-posframe-display-functions-alist
    '((swiper                     . ivy-posframe-display-at-point)
      (complete-symbol            . ivy-posframe-display-at-point)
      (counsel-M-x                . ivy-display-function-fallback)
      (counsel-esh-history        . ivy-posframe-display-at-window-center)
      (counsel-describe-function  . ivy-display-function-fallback)
      (counsel-describe-variable  . ivy-display-function-fallback)
      (counsel-find-file          . ivy-display-function-fallback)
      (counsel-recentf            . ivy-display-function-fallback)
      (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
      (dmenu                      . ivy-posframe-display-at-frame-top-center)
      (nil                        . ivy-posframe-display))
    ivy-posframe-height-alist
    '((swiper . 20)
      (dmenu . 20)
      (t . 10)))
  :config
  (ivy-posframe-mode 1)) ; 1 enables posframe-mode, 0 disables it.

(use-package projectile
:config
(projectile-mode +1))

(use-package evil
:init      ;; tweak evil's configuration before loading it
;;  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
(evil-mode)
)
(use-package evil-collection
 :after evil
 :config
 (setq evil-collection-mode-list '(dashboard dired ibuffer))
 (evil-collection-init))
 (use-package evil-tutor)
 (evil-mode t)

(setq bare-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
(setq bare-work-tree (concat "--work-tree=" (expand-file-name "~")))
;; use maggit on git bare repos like dotfiles repos, don't forget to change `bare-git-dir' and `bare-work-tree' to your needs
(defun me/magit-status-bare ()
"set --git-dir and --work-tree in `magit-git-global-arguments' to `bare-git-dir' and `bare-work-tree' and calls `magit-status'"
(interactive)
(require 'magit-git)
(add-to-list 'magit-git-global-arguments bare-git-dir)
(add-to-list 'magit-git-global-arguments bare-work-tree)
(call-interactively 'magit-status))

;; if you use `me/magit-status-bare' you cant use `magit-status' on other other repos you have to unset `--git-dir' and `--work-tree'
;; use `me/magit-status' insted it unsets those before calling `magit-status'
(defun me/magit-status ()
"removes --git-dir and --work-tree in `magit-git-global-arguments' and calls `magit-status'"
(interactive)
(require 'magit-git)
(setq magit-git-global-arguments (remove bare-git-dir magit-git-global-arguments))
(setq magit-git-global-arguments (remove bare-work-tree magit-git-global-arguments))
(call-interactively 'magit-status))

(use-package magit)

;;(use-package recentf
;;    :config
;;    (recentf-mode))
(use-package sudo-edit) ;; Utilities for opening files with sudo

(use-package company)
(company-mode)

(use-package cdlatex)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

(use-package markdown-mode)
(use-package web-mode)

(use-package plantuml-mode)
;; Sample jar configuration
(setq plantuml-jar-path "/opt/plantuml-1.2023.4.jar")
(setq plantuml-default-exec-mode 'jar)

(use-package lsp-mode)
(use-package lsp-ui)

;; Function for setting a fixed width for neotree.
;; Defaults to 25 but I make it a bit longer (35) in the 'use-package neotree'.
(defcustom neo-window-width 25
  "*Specifies the width of the NeoTree window."
  :type 'integer
  :group 'neotree)

(use-package neotree
  :config
  (setq neo-smart-open t
        neo-window-width 30
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        ;;neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
        ;; truncate long file names in neotree
        (add-hook 'neo-after-create-hook
           #'(lambda (_)
               (with-current-buffer (get-buffer neo-buffer-name)
                 (setq truncate-lines t)
                 (setq word-wrap nil)
                 (make-local-variable 'auto-hscroll-mode)
                 (setq auto-hscroll-mode nil)))))

;; show hidden files
(setq-default neo-show-hidden-files t)

(nvmap :prefix "SPC"
       "t n"   '(neotree-toggle :which-key "Toggle neotree file viewer")
       "d n"   '(neotree-dir :which-key "Open directory in neotree"))

(nvmap :prefix "SPC"
       "r c"   '(copy-to-register :which-key "Copy to register")
       "r f"   '(frameset-to-register :which-key "Frameset to register")
       "r i"   '(insert-register :which-key "Insert register")
       "r j"   '(jump-to-register :which-key "Jump to register")
       "r l"   '(list-registers :which-key "List registers")
       "r n"   '(number-to-register :which-key "Number to register")
       "r r"   '(counsel-register :which-key "Choose a register")
       "r v"   '(view-register :which-key "View a register")
       "r w"   '(window-configuration-to-register :which-key "Window configuration to register")
       "r +"   '(increment-register :which-key "Increment register")
       "r SPC" '(point-to-register :which-key "Point to register"))

(use-package all-the-icons-dired)
(use-package dired-open)
(use-package peep-dired)
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
                "d d" '(dired :which-key "Open dired")
                "d j" '(dired-jump :which-key "Dired jump to current")
                "d p" '(peep-dired :which-key "Peep-dired"))

(with-eval-after-load 'dired
(define-key dired-mode-map (kbd "M-p") 'peep-dired)
(evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
(evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
(evil-define-key 'normal peep-dired-mode-map (kbd "n") 'peep-dired-next-file)
(evil-define-key 'normal peep-dired-mode-map (kbd "p") 'peep-dired-prev-file))

(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                            ("jpg" . "sxiv")
                            ("png" . "sxiv")
                            ("mkv" . "mpv")
                            ("mp4" . "mpv")
                            ("pdf" . "evince")))

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
        "."     '(find-file :which-key "Find file")
        "f f"   '(find-file :which-key "Find file")
        "f r"   '(counsel-recentf :which-key "Recent files")
        "f s"   '(save-buffer :which-key "Save file")
        "f u"   '(sudo-edit-find-file :which-key "Sudo find file")
        "f y"   '(dt/show-and-copy-buffer-path :which-key "Yank file path")
        "f c"   '(copy-file :which-key "Copy file")
        "f d"   '(delete-file :which-key "Delete file")
        "f R"   '(rename-file :which-key "Rename file")
        "f S"   '(write-file :which-key "Save file as...")
        "f U"   '(sudo-edit :which-key "Sudo edit file"))

(defun dt/show-and-copy-buffer-path ()
"Show and copy the full path to the current file in the minibuffer."
(interactive)
;; list-buffers-directory is the variable set in dired buffers
(let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
    (error "Buffer not visiting a file"))))
(defun dt/show-buffer-path-name ()
"Show the full path to the current file in the minibuffer."
(interactive)
(let ((file-name (buffer-file-name)))
    (if file-name
        (progn
        (message file-name)
        (kill-new file-name))
    (error "Buffer not visiting a file"))))

;; display line numbers
(require 'display-line-numbers)


(defcustom display-line-numbers-exempt-modes
'(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
"Major modes on which to disable line numbers."
:group 'display-line-numbers
:type 'list
:version "green")

(defun display-line-numbers--turn-on ()
"Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
(unless (or (minibufferp)
            (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))
(global-display-line-numbers-mode)
(setq column-number-mode t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell t)
(delete-selection-mode t)

;; set theme and font
;; (load-theme 'tsdh-dark)
(use-package dracula-theme)
;(load-theme 'dracula t)

(use-package doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
    doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)

(use-package doom-modeline)
(doom-modeline-mode 1)



(set-face-attribute 'default nil :font "Fira Code")

(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)

(use-package all-the-icons)

  
(use-package dashboard
    :ensure t
    :init      ;; tweak dashboard config before loading it
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
    ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
    (setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
    (setq dashboard-center-content nil) ;; set to 't' for centered content
    (setq dashboard-items '((recents . 5)
			    (agenda . 5 )
			    (bookmarks . 3)
			    (projects . 3)
			    (registers . 3)))
    :config
    (dashboard-setup-startup-hook)
    (dashboard-modify-heading-icons '((recents . "file-text")

				      (bookmarks . "book"))))
  ; ensure emacs open in dashboard
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq ivy-initial-inputs-alist nil)

(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]  
             [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
             [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
             [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
             [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
  (global-unset-key k))

;; An example of how this works.
;; [[arch-wiki:Name_of_Page][Description]]
(setq org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        '(("google" . "http://www.google.com/search?q=")
          ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
          ("ddg" . "https://duckduckgo.com/?q=")
          ("wiki" . "https://en.wikipedia.org/wiki/")))

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook 'org-indent-mode)
(setq org-directory "~/Documents/Org/"
      org-agenda-files '("~/Documents/Org/")
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-default-todos-file (expand-file-name "todos.org" org-directory)
      org-ellipsis " ▼ "
      org-log-done 'time
      org-journal-dir "~/Documents/Or2/journal/"
      org-journal-date-format "%B %d, %Y (%A) "
      org-journal-file-format "%Y-%m-%d"
      org-default-journal-file (concat (expand-file-name (format-time-string org-journal-file-format) org-journal-dir) ".org")
      org-hide-emphasis-markers t)
(setq org-src-preserve-indentation nil
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-capture-templates
      '(
            ("t" "Todo" entry (file+headline org-default-todos-file "Tasks.org")
                "* TODO %?\n  %i\n  %a")
            ("n" "Note" entry (file+headline org-default-notes-file "Notes.org")
                "* %?\n  %i\n  %a")
            ("j" "Journal" entry (file+datetree org-default-journal-file)
                "* %?\nEntered on %U\n  %i\n  %a")
         ))

(setq org-roam-capture-templates
      '(
        ("f" "Fleeting note" plain "%?"
        :target (file+head "01_FLEETING_NOTES/%<%Y%m%d%H%M%S>-${slug}.org"
            "#+title: ${title}\n")
            :unnarrowed t)
        ("l" "Literature note" plain "%?"
          :target (file+head "02_LITERATURE_NOTES/%<%Y%m%d%H%M%S>-${slug}.org"
          "#+title: ${title}\n") :unnarrowed t)
        ("p" "Permanent note" plain "%?"
          :target (file+head "03_PERMANENT_NOTES/%<%Y%m%d%H%M%S>-${slug}.org"
          "#+title: ${title}\n") :unnarrowed t)
        ("i" "Index note" plain "%?"
          :target (file+head "04_INDEX_NOTES/%<%Y%m%d%H%M%S>-${slug}.org"
          "#+title: ${title}\n") :unnarrowed t)
        )
      )

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c r o") #'org-roam-node-find)
(global-set-key (kbd "C-c r i") #'org-id-get-create)

(use-package org-journal
    :ensure nil)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package ox-man
  :ensure nil)

(use-package ox-gfm)

(use-package org-roam)
(setq org-roam-directory (file-truename "~/Documents/Org2/"))

(use-package org-roam-ui)

(org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
))

(use-package org-tempo
  :ensure nil) ;; tell use-package not to try to install org-tempo since it's already there.

(define-skeleton journla-skeleton
"This is a template to be launch in journal-mode"
"* DAY: " str | " insert day " " \n"
"_____"
"* Objetivos del día\n\n"
"* Cosas buenas de hoy\n\n"
"* Algo que haya aprendido\n\n"
"* Objetivos para mañana\n")
;; launch the skeleton with ^C-jr

(add-hook  'text-mode-hook (lambda () (flyspell-mode)))
