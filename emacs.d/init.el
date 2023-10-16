(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(org-babel-load-file
(expand-file-name "config.org" "~/.emacs.d/"))

