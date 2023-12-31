;;; lem_conf.el --- define some functions  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains some functions and hooks that I define

;;; Code:


(defgroup lem ()
  "Group for some personal variables."
  :prefix 'lem
  :version '0.0.1)

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


(defvar lem/sync_script_path
  "The path for the script used to sync the files."
  (let ((file-name (expand-file-name "sync.sh" "~/.local/bin/")))
    (if (file-exists-p file-name)
        file-name
      nil)))

(defcustom lem/dotfiles "~/Documents/git/dotfiles/"
  "The path where the dotfiles git repo is stored."
  :group 'lem
  :type '(string))

(defcustom lem/bibliography-files '("~/Documents/Org/bibliography.bib" "~/Documents/Org/phd.bib")
  "List of the .bib to get the bibliography."
  :group 'lem
  :type '(list)
  )

(defcustom lem/fill-column 120
  "The default number of columns used for wrap the text."
  :group 'lem
  :type '(number))

(defun lem/sync (path)
  "Call the sync comand with the project to be syncrhonize.
PATH: is the dir where the git repo is"
  (shell-command-to-string (concat lem/sync_script_path path)))

(defun lem/syc-org ()
  "Sync the Org directory with an external script."
  (interactive)
  (lem/sync org-directory))

(defun lem/sync-conf ()
  "Sync the config foler with an external script."
  (interactive)
  (lem/sync lem/dotfiles))


(provide 'lem_conf)
;;; lem_conf.el ends here
