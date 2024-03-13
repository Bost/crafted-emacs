(setq-local context "early-init:") ;; for log messages

(message "%s user-emacs-directory %s" context user-emacs-directory)
(load (expand-file-name "modules/crafted-early-init-config"
                        user-emacs-directory))
