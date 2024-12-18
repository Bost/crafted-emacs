(setq-local context "early-init:") ;; for log messages

;; 1. '--maximized --fullscreen' in:
;;   /home/bost/.guix-profile/bin/emacs --maximized --fullscreen --with-profile=crafted --daemon & disown
;; don't work.
;; Emacs reads your main init file after creating the initial frame. However:
;; 2. Setting just '((fullscreen . maximized)) in initial-frame-alist doesn't work.
;;(setq initial-frame-alist '((fullscreen . maximized)))
;; 3 Setting default-frame-alist doesn't works (inspired by Spacemacs).
(setq default-frame-alist
      `(
        (icon-type . ,(concat user-emacs-directory
                              ;; "personal/emacs_red.png"
                              ;; "personal/emacs_green.png"
                              ;; "personal/emacs_yellow.png"
                              "system-crafters-logo.png"))
        (fullscreen . maximized)
        (internal-border-width . 0)
        (undecorated . t)
        (vertical-scroll-bars)
        ))

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

;; user-emacs-directory is changed at the end of init.el from
;;   "/home/bost/.emacs.d.distros/crafted-emacs/" to "/home/bost/.cache/emacs"
;; after the start. Why?!?
(setq crafted-emacs-dir user-emacs-directory)

(load (expand-file-name "modules/crafted-early-init-config"
                        user-emacs-directory))
