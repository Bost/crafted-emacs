(setq-local context "early-init:") ;; for log messages

;; Prevent warning what byte compilation fails - doesn't work
;;   Warning (comp): Error: Symbol's function definition is void make-tramp-file-name
;; See also https://elpa.gnu.org/packages/tramp.html
;; (when (string-equal emacs-version "29.1")
;;        (with-current-buffer
;;            (url-retrieve-synchronously
;;             "https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/loaddefs-gen.el?h=emacs-29")
;;          (goto-char (point-min))
;;          (while (looking-at "^.+$") (forward-line))
;;          (eval-region (point) (point-max))))

(message "%s user-emacs-directory %s" context user-emacs-directory)
(load (expand-file-name "modules/crafted-early-init-config"
                        user-emacs-directory))
