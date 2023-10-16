(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-single-buffer ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt
  (kbd "y") 'dired-ranger-copy
  (kbd "p") 'dired-ranger-paste
  (kbd "d") 'dired-ranger-move)

(setq dired-listing-swithces "--group-directories-first"
      delete-by-moving-to-trash t
      dired-open-extensions '(("gif" . "eog")
                              ("jpg" . "eog")
                              ("png" . "eog")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")
                              ("pdf" . "evince")))

(setq doom-font (font-spec :family "Fira Code" :size 15)
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 15)
      doom-big-font (font-spec :family "Fira Code" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

;;(after! magit
;;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))



(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode
                (list 'org-agenda-mode)))
     (rainbow-mode 1))))
(global-rainbow-mode 1 )

(after! ivy
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
)

(use-package! ivy
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
  :config
  (ivy-mode 1))

(defun lem/switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
     (change (if (string= dic "en_US") "es_ES" "en_US")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

  (global-set-key (kbd "<f8>")   'lem/switch-dictionary)

(defun lem/text-mode-setup ()
  "test hook"
  (variable-pitch-mode 1)
  (auto-fill-mode 1)
  ;(visual-line-mode 1)
  (visual-fill-column-mode 1)
  (setq evil-auto-indent nil))

(add-hook 'tex-mode-hook 'lem/text-mode-setup)

(setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-process-alist
	  '((dvipng :programs
		    ("latex" "dvipng")
		    :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
		    (1.0 . 1.0)
		    :latex-compiler
		    ("latex -interaction nonstopmode -output-directory %o %f")
		    :image-converter
		    ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
	    (dvisvgm :programs
		     ("latex" "dvisvgm")
		     :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
		     (1.7 . 1.5)
		     :latex-compiler
		     ("latex -interaction nonstopmode -output-directory %o %f")
		     :image-converter
		     ("dvisvgm %f -e -n -b min -c %S -o %O"))
	    (imagemagick :programs
			 ("latex" "convert")
			 :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
			 (1.0 . 1.0)
			 :latex-compiler
		 ("pdflatex -interaction nonstopmode -output-directory %o %f")
			 :image-converter
			 ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

(map! :leader
      :desc "Org babel tangle" "m B" #'org-babel-tangle)
(defun lem/org-mode-setup ()
  (org-indent-mode))

(after! org
(setq
      org-directory "~/Documents/Org"
      org-default-notes-file (expand-file-name "Inbox.org" org-directory)
      org-ellipsis " ▾"
      org-superstar-headline-bullets-list '("◉" "●" "○" "◆" "●" "○" "◆")
      org-log-done 'time
      org-hide-emphasis-markers nil
      org-src-fontify-natively t
      org-fontify-quote-and-verse-blocks t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 2
      org-hide-block-startup nil
      org-src-preserve-indentation nil
      org-cycle-separator-lines 2
      org-todo-keywords '((sequence
                           "TODO(t)" "NEXT(n)" "WAIT(w)" "|"
                           "DONE(d!)" "CANCELED(c)"))
      org-tag-alist
      '((:startgroup . nil)
        ;; Put mutually exclusive tags here
        ("@home" . ?H )
        ("@PHD" . ?P)
        ("@UI" . ?U)
        (:endgroup . nil))
      org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 1))
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path    t
      user-full-name "Lucas Elvira Martín"
      user-mail-address "lucaselvira96@gmail.com"))

(setq org-agenda-files (mapcar (lambda (file) (concat org-directory file)) '("Tasks.org" "Habits.org")))

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 'week)
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

; custom display of task
(setq org-columns-default-format "%20CATEGORY(Category) %30ITEM(Task) %4TODO %6Effort(Estim){:} %16SCHEDULED %6CLOCKSUM(Clock) %TAGS")

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tl" "Task with link" entry (file+olp+datetree ,(concat org-directory "Tasks.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("tt" "Task" entry (file+olp+datetree ,(concat org-directory "Tasks.org"))
         "* TODO %?\n  %U\n  %i"
         :tree-type week
         :empty-lines 1)
      ("j" "Journal Entries")
      ("je" "Journal" entry
        (file+olp+datetree ,(concat org-directory "Journal.org"))
        "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
        :tree-type week
        :clock-in :clock-resume
        :empty-lines 1)
      ("jt" "Task Entry" entry
       (file+olp+datetree ,(concat org-directory "Journal.org"))
       "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
       :tree-type week
       :clock-in :clock-resume
       :empty-lines 1)
      ("jm" "Meeting" entry
       (file+olp+datetree ,(concat org-directory "Journal.org"))
       "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
       :clock-in :clock-resume
       :empty-lines 1)
      ("jj" "Journal" entry
       (file+olp+datetree ,(concat org-directory "Journal.org"))
       "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
       :tree-type week
       :clock-in :clock-resume
       :empty-lines 1)))

(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil  :weight 'medium :height (cdr face)))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("js" . "src python"))
(add-to-list 'org-structure-template-alist '("ex" . "export"))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default nil))

(after! ox-latex

  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
(setq org-latex-listings t)

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

(after! org
  (setq org-roam-directory (file-truename "~/Documents/Org/roam")
))

(map! :leader
      (:prefix ("n r" . "org-roam")
       :desc "Completion at point" "c" #'completion-at-point
       :desc "Find node"           "f" #'org-roam-node-find
       :desc "Show graph"          "g" #'org-roam-graph
       :desc "Insert node"         "i" #'org-roam-node-insert
       :desc "Capture to node"     "n" #'org-roam-capture
       :desc "Toggle roam buffer"  "r" #'org-roam-buffer-toggle))

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+author: %n\n")
         :unnarrowed t)
        ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
:unnarrowed t)
        ))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(require 'org-habit)
 (add-to-list 'org-modules 'org-habit)
 (setq org-habit-graph-column 60)

(use-package! org-pomodoro
  :after org
  :commands (org-pomodoro)
  :config
  (setq
 alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
 org-pomodoro-length 120
 org-pomodoro-short-break-length 10))

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

          ;; Build the agenda list the first time for the session

        (defun my/org-roam-project-finalize-hook ()
            "Adds the captured project file to `org-agenda-files' if the
          capture was not aborted."
            ;; Remove the hook since it was added temporarily
            (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

            ;; Add project file to the agenda list if the capture was confirmed
            (unless org-note-abort
              (with-current-buffer (org-capture-get :buffer)
                (add-to-list 'org-agenda-files (buffer-file-name)))))

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

  (defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))
(defun my/org-roam-capture-task ()
  (interactive)
  ;; Capture the new task, creating the project file if necessary
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
                                  )))

(defun lem/org-roam-capture-meeting ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-read
                           nil
                           (my/org-roam-filter-by-tag "Project"))
                     :templates '(("m" "Meeting" plain "** %<%Y-%m-%d %H:%M>\n%?"
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                              "#+title: ${title}\n#+category:${title}\n#+filetags: Project"
                                                              ("Meetings"))
                                   :clock-in :clock-resume
                                   :empty-lines: 1))))

(add-to-list 'org-roam-capture-templates
             '("f" "Fleeting" plain "%?"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+DATE: %U\n#+AUTHOR: %n\n#+filetags: fleeting")
         :unnarrowed nil))

(setq bibliography-files '("~/Documents/Org/bibliography.bib"
                               "~/Documents/Org/phd.bib"))
;;   (after! ivy-bibtex
;;     (setq bibtex-completion-bibliography bibliography-files))


  (use-package! org-ref
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
    )


  (use-package! org-ref-ivy
    :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
                org-ref-insert-cite-function 'org-ref-cite-insert-ivy
                org-ref-insert-label-function 'org-ref-insert-label-link
                org-ref-insert-ref-function 'org-ref-insert-ref-link
                org-ref-cite-onclick-function (lambda (_)
                  (org-ref-citation-hydra/body))))

(use-package! citar
  :custom
  (citar-bibliography bibliography-files))

(use-package! org-roam-bibtex
  :after org-roam)
(use-package! citar-org-roam
 :after citar-org-roam
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
 (setq citar-notes-source 'orb-citar-source))

(setq citar-org-roam-note-title-template "${author} - ${title}")
(add-to-list 'org-roam-capture-templates
             '("r" "bibliography reference" plain "%?"
               :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                  "#+TITLE: ${title}\n#+AUTHOR: ${author}\n#+filetags: Literature\n#+cite-key: ${citekey}\n#+cite-date: ${date} \n#+created: %U\n\n* ${title}\n\n**Abstract\n${abstract}")
               :unnarrowed t))
(setq citar-org-roam-capture-template-key "r")

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
nil "~/Documents/Org/roam/glossary2.tex" 'append)))
