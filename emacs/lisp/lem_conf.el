;;; lem_conf.el --- define some functions  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains some functions and hooks that I define

;;; Code:

(defgroup lem ()
  "Group for some personal variables."
  :group 'emacs
  :prefix 'lem
  :version '0.0.1)

(defcustom lem/sync_script_path
  (let ((
         file-name (expand-file-name "sync.sh" "~/.local/bin/")))
    (if (file-exists-p file-name) file-name nil))
  "The path where the sync file is stored."
  :group 'lem
  :type '(file :must-match t))

(defcustom lem/dotfiles "~/Documents/git/dotfiles/"
  "The path where the dotfiles git repo is stored."
  :group 'lem
  :type '(directory :must-match t))

(defcustom lem/bibliography-files '("~/Documents/Org/bibliography.bib" "~/Documents/Org/phd.bib")
  "List of the .bib to get the bibliography."
  :group 'lem
  :type '(repeat :tag "List of bib files" file :must-match t))

(defcustom lem/org-directory "~"
  "The directory for the Org repository."
  :set (lambda (k v)
         (set-default k v)
         (setq org-directory v
               org-default-notes-file (concat org-directory "Inbox.org")
               org-roam-directory (expand-file-name "roam" org-directory)))
  :group 'lem
  :type '(directory :must-match t))

(defcustom lem/alpha-value 90
  "The default value of transparency used for the current frame."
  :set (lambda (k v)
         (set-default k v)
         (when (fboundp 'lem/set-background) (lem/set-background)))
  :group 'lem
  :type '(number))

(defvar lem/is-transparent t
  "If the frame is transparent or not.")

(defun lem/delete-this-file ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename "?"))
            (progn (delete-file filename)
                   (message "File delete")
                   (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun lem/rename-this-file ()
  "Rename the current file."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


(defun lem/sync (path)
  "Call the sync comand with the project to be syncrhonize.
PATH: is the dir where the git repo is"
  (shell-command-to-string (concat lem/sync_script_path " " path)))

(defun lem/sync-org ()
  "Sync the Org directory with an external script."
  (interactive)
  (lem/sync org-directory))

(defun lem/sync-conf ()
  "Sync the config foler with an external script."
  (interactive)
  (lem/sync lem/dotfiles))

(defun lem/set-background ( &optional frame)
  (unless is-termux
    (let ((alpha (if (boundp 'lem/alpha-value) lem/alpha-value 100)))
      (let ((tuple `(,alpha . ,alpha)))
        (set-frame-parameter frame 'alpha tuple)
        (add-to-list 'default-frame-alist '(alpha-background . lem/alpha-value))))))

(defun lem/toggle-transparency ()
  "Toggle the state of the transparency for all the frames."
  (interactive)
  (let ((alpha (if lem/is-transparent 100 lem/alpha-value)))
    (let ((tuple `(,alpha . ,alpha)))
      (set-frame-parameter nil 'alpha tuple)
      (add-to-list 'default-frame-alist '(alpha-background . lem/alpha-value))))
  (setq-default lem/is-transparent (not lem/is-transparent)))


(provide 'lem_conf)
;;; lem_conf.el ends here
