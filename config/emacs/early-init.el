(defvar private-emacs-directory nil
"The path where the configuration file for emacs are stored.")
(setq package-enable-at-startup nil
      private-emacs-directory user-emacs-directory
    ;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
      user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(setq url-history-file (expand-file-name "url/history" user-emacs-directory))

(setq site-run-file nil                         ; No site-wide run-time initializations.
      inhibit-default-init t                    ; No site-wide default library
      gc-cons-threshold (* 50 1000 1000) ; The default is 800 kilobytes. Measured in bytes.
      native-comp-eln-load-path (list (expand-file-name "eln-cache" user-emacs-directory)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections." (emacs-init-time "%.2f") gcs-done)
            (setq gc-cons-threshold (* 8 1024 1024))))
