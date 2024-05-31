(setq-local context "init:") ;; for log messages

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load (concat crafted-emacs-dir "modules/crafted-init-config"))

(setq-local
 modules-and-packages
 '(
   crafted-early-init-config
   crafted-defaults-config    ; Sensible default settings for Emacs
   crafted-updates-config     ; Tools to upgrade Crafted Emacs
   crafted-completion-config  ; selection framework based on `vertico`
   crafted-package-config
   crafted-evil-config        ; An `evil-mode` configuration
   crafted-org-config         ; org-appear, clickable hyperlinks etc.
   crafted-speedbar-config
   ;; crafted-startup-config
   crafted-workspaces-config
   crafted-completion-config
   crafted-lisp-config
   crafted-ide-config
   crafted-ui-config ; Better UI experience (modeline etc.-config)
   ;; require crafted-compile-config     ; automatically compile some emacs lisp files
   ;; crafted-init-config
   ;; crafted-updates-config
   ;; crafted-osx-config
   ;; crafted-screencast-config  ; show current command and binding in modeline
   ;; ;; yasnippet
   ;; ;; yasnippet-snippets
   ;; haskell-snippets
   jump-last
   kill-buffers
   tweaks
   spacemacs-auto-highlight
   terminals
   ;; frame ;; for toggle-frame-fullscreen
   ))

(dolist (package modules-and-packages)
  ;; message does the prettyfing, we need to compose `print after format` functions
  ;; (print (format "%s (require '%s) ..." context package))
  (message "%s (require '%s) ..." context package)
  (require package))
;; (message "%s all requires done" context)

(crafted-package-install-package 'doom-themes)
(progn
  ;; (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
  ;; (load-theme 'doom-palenight t)     ; load the doom-palenight theme
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-solarized-light t)
  )

(defun spacemacs-mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.

If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

(defun set-default-font (plists)
  "spacemacs/set-default-font from
 /home/bost/.emacs.d.distros/spacemacs/core/core-fonts-support.el

Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (message "%s (font-spec :name (car plist)) %s" context
               (font-spec :name (car plist)))
      (message "%s (find-font (font-spec :name (car plist))) %s"
               context (find-font (font-spec :name (car plist))))
      (when (find-font (font-spec :name (car plist)))
        (message "%s find-font" context)
        (let* ((font (car plist))
               (props (cdr plist))
               ;; TODO there's a bug in spacemacs-mplist-remove
               (font-props (spacemacs-mplist-remove props :powerline-offset))
               (fontspec (apply 'font-spec :name font font-props)))
          (message "%s Setting font \"%s\"..." context font)
          ;; We set the INHIBIT-CUSTOMIZE parameter to t to tell set-frame-font
          ;; not to fiddle with the default face in the user's Customization
          ;; settings. We don't need Customization because our way of ensuring
          ;; that the font is applied to future frames is to modify
          ;; default-frame-alist, and Customization causes issues, see
          ;; https://github.com/syl20bnr/spacemacs/issues/5353.
          ;; INHIBIT-CUSTOMIZE is only present in recent emacs versions.
          (if (version< emacs-version "28.0.90")
              (set-frame-font fontspec nil t)
            (set-frame-font fontspec nil t t))
          (push `(font . ,(frame-parameter nil 'font)) default-frame-alist)
          ;; fallback font for unicode characters used in spacemacs
          (setq fallback-font-name "NanumGothic")
          (setq fallback-font-name2 "NanumGothic")
          ;; remove any size or height properties in order to be able to scale
          ;; the fallback fonts with the default one (for zoom-in/out for
          ;; instance)
          (let* ((fallback-props (spacemacs-mplist-remove
                                  (spacemacs-mplist-remove font-props :size)
                                  :height))
                 (fallback-spec (apply 'font-spec
                                       :name fallback-font-name
                                       fallback-props)))
            (message "fallback-props %s" fallback-props)
            (message "fallback-spec %s" fallback-spec)
            ;; window numbers (ding bang circled digits)
            (set-fontset-font "fontset-default"
                              '(#x2776 . #x2793) fallback-spec nil 'prepend)
            ;; mode-line circled letters (circled latin capital/small letters)
            (set-fontset-font "fontset-default"
                              '(#x24b6 . #x24e9) fallback-spec nil 'prepend)
            ;; mode-line additional characters (circled/squared
            ;; mathematical operators)
            (set-fontset-font "fontset-default"
                              '(#x2295 . #x22a1) fallback-spec nil 'prepend)
            ;; new version lighter (arrow block)
            (set-fontset-font "fontset-default"
                              '(#x2190 . #x21ff) fallback-spec nil 'prepend)))
        (message "%s font set" context)
        (throw 'break t)))
    (progn
      (message "%s find-font: false" context)
      nil)))

;; (window-parameter (selected-window) 'no-delete-other-windows)

;; (setq default-font
;;       `("Source Code Pro"
;;         :size
;;         ,(let* ((hostname (system-name))
;;                 (default 10.0)
;;                 ;; The `text-scale-mode-step' is not accessible at this moment.
;;                 (text-scale-mode-step 1.2)
;;                 (point-size (cond
;;                              ((string= hostname "edge") 19.0)
;;                              ((or (string= hostname "ecke")
;;                                   (string= hostname "tuxedo"))
;;                               ;; (+ default (* 6 text-scale-mode-step)) ; 17.2
;;                               (+ default (* 7 text-scale-mode-step)) ; 18.4
;;                               )
;;                              ;; TODO this is a pixel-size not point-size
;;                              ((string= hostname "geek") 17)
;;                              (t default))))
;;            (message "%s default-font hostname: %s; point-size: %s"
;;                     context hostname point-size)
;;            point-size)
;;         :weight normal
;;         :width normal))

(defun set-default-font-prot ()
  (set-face-attribute
   'default nil
   :family "Source Code Pro"
   :height (let* ((hostname (system-name))
                  (default 10.0)
                  ;; The `text-scale-mode-step' is not accessible at this moment.
                  (text-scale-mode-step 1.2)
                  (point-size (cond
                               ((string= hostname "edge") 19.0)
                               ((or (string= hostname "ecke")
                                    (string= hostname "tuxedo"))
                                ;; (+ default (* 6 text-scale-mode-step)) ; 17.2
                                (+ default (* 7 text-scale-mode-step)) ; 18.4
                                )
                               ;; TODO this is a pixel-size not point-size
                               ((string= hostname "geek") 17)
                               (t default))))
             (message "%s default-font hostname: %s; point-size: %s"
                      context hostname point-size)
             (truncate (* 10 point-size)))
   :weight 'normal))

;; (message "%s set-default-font ..." context)
;; (if (set-default-font default-font)
;;     (message "%s set-default-font ... done" context)
;;     (progn
;;       (message "%s set-default-font ... failed" context)
;;       (message "%s set-default-font-prot ... done" context)
;;       (set-default-font-prot)
;;       (message "%s set-default-font ... done" context)))

(message "%s set-default-font-prot ... done" context)
(set-default-font-prot)
(message "%s set-default-font ... done" context)

(defun my-shell-readlink (file)
  "Execute the `readlink FILE` command in the current shell."
  (funcall
   (-compose
    ;; TODO implement fallback to bash if fish not found
    #'string-trim-right
    #'shell-command-to-string
    (lambda (strings) (string-join strings " "))
    (-partial #'list "readlink"))
   file))

(defun my-delete-other-windows ()
  "docstring"
  (interactive)
;;;;  ;; See definitions of `treemacs'
;;;;  (pcase (treemacs-current-visibility)
;;;;    ('visible (delete-window (treemacs-get-local-window)))
;;;;    ;; ('exists  (treemacs-select-window))
;;;;    ;; ('none    (treemacs--init))
;;;;    )
  (delete-other-windows)
  )

(defun my-racket-repl-clear ()
  (interactive)
  (let ((inhibit-read-only t))
    ;; (erase-buffer)
    (delete-region (point-min) (- (point-max) 2))))

(defun my-H-1 () (interactive) (message "H-1"))
(defun my-H-2 () (interactive) (message "H-2"))
(defun my-H-3 () (interactive) (message "H-3"))
(defun my-H-4 () (interactive) (message "H-4"))

;;; beg: from spacemacs/layers/+spacemacs/spacemacs-defaults/keybindings.el
(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))
;;; end: from spacemacs/layers/+spacemacs/spacemacs-defaults/keybindings.el

(defun my-eval-bind-keys-and-chords ()
  "To activate changes, do:
    ~s-d~ my-eval-current-defun
    ~s-+~ my-eval-bind-keys-and-chords

Some binding snippets / examples:
  (global-set-key (kbd \"s-<f2>\") \\='eshell)
  (key-chord-define-global \"fj\" (lambda () (interactive)
                                             (tw-insert-str \"()\" 1)))"
  (interactive)

  ;; (key-chord-define-global "fj" (lambda () (interactive)
  ;;                                 (tw-insert-str "()" 1)))
  ;; (key-chord-define clojure-mode-map "fj" nil)
  ;; (key-chord-define global-map "fj" nil)

  ;; see also `key-chord-unset-global' / `key-chord-unset-local'

  ;; for the substitution: ~s-:~ / M-x tw-fabricate-subst-cmd
  ;; TODO this keychord doesn't work
  (bind-chords :map evil-ex-completion-map ; not he evil-ex-map!!!
               ("()" . tw-insert-group-parens))
  (bind-keys :map evil-ex-completion-map ; not he evil-ex-map!!!
             ("s-0" . tw-insert-group-parens)
             ("s-)" . tw-insert-group-parens))

  ;; (setq evil-respect-visual-line-mode t) doesn't work easily
  ;; `remap' can't use the #' reader syntax for function form
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
  (global-set-key [remap evil-beginning-of-line] #'crux-move-beginning-of-line)

  ;; BUG: The ~C-z~ / M-x suspend-frame surfaces when calling
  ;; ~SPC k e~ / M-x evil-lisp-state-sp-splice-sexp-killing-forward.
  ;; since this function changes 'evil-state' to 'lisp'.
  ;; ~ESC~ / M-x evil-lisp-state/quit
  (global-unset-key (kbd "C-z"))

  ;; See also the value of `dotspacemacs-smart-closing-parenthesis'
  ;; (unbind-key ")" term-mode-map)

  (bind-chords :map global-map
               ("KK" . tw-switch-to-previous-buffer)
               ;; don't need to switch keyboards just because of parenthesis
               ("fj" . (tw-insert-str "()" 1)))
  (bind-keys ; :map global-map
   :map global-map
   ("<f5>" . tw-revert-buffer-no-confirm)
   ;; ("s-*"    . er/contract-region) ;; TODO see https://github.com/joshwnj

   ;; TODO The <escape> keybinding seems not to work.
   ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
   ;; TODO notmuch

   ;; TODO tw-emacs-comment-sexp: mark-sexp C-M-@ comment-dwim M-;

   ;; the funny keys can be seen
   ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2008-11/msg00011.html
   ("C-s-<268632070>" . my-H-3) ;; this is probably for an Apple computers
   ("<escape>"  . keyboard-escape-quit)

   ("M-Q"       . unfill-paragraph)
   ;; TODO in the fish-mode the ~M-q~ is bound to `paredit-reindent-defun',
   ;; which doesn't work well for unfilling comments (filling works). The same
   ;; goes for `unfill-toggle'. (Does `paredit-reindent-defun' call
   ;; `unfill-toggle'?)
   ;; ("M-q" . unfill-toggle)

   ("s-="       . balance-windows-area)
   ("s-K"       . kb-kill-buffers--unwanted)
   ("s-C-K"     . kb-kill-buffers--dired)
;;;;     ("s-R"       . spacemacs/rename-current-buffer-file)
   ("s-q"       . tw-other-window)
   ("s-k"       . kb-close-buffer)
   ("s-s"       . save-buffer)
   ("s-0"       . tw-delete-window)
   ("s-1"       . my-delete-other-windows)
   ("S-s-<f8>"    . ace-swap-window)
   ;; ("S-s-<f8>" . transpose-frame)
;;;;     ("s-N"       . spacemacs/cycle-defun-narrow-modes)
;;;;     ("s-n"       . spacemacs/cycle-narrow-widen)
   ;; ("s-2"    . tw-split-other-window-below)
   ("s-2"       . split-window-below) ; ~SPC w -~
   ;; see ~SPC w /~ and ~SPC w 2~
   ;; ("s-3"    . spacemacs/window-split-double-columns)
   ;; see ~SPC w /~ and ~SPC w 2~
   ("s-3"       . split-window-right-and-focus)
;;;;     ("s-9"       . my-load-layout)
   ("s-+"       . tw-eval-bind-keys-and-chords)
   ("s-<kp-add>". tw-eval-bind-keys-and-chords)
   ("s-z"       . tw-buffer-selection-show)
   ;; dired: https://danlamanna.com/forget-scp-use-dired-dwim.html
   ("s-D"       . dired-jump) ;; just open a dired buffer

   ;; The highlighting of copied sexps is done by the copy-sexp.el
   ;; TODO highlight text yanked with `y'
   ("s-c"       . sp-copy-sexp)
   ("s-b"       . sp-backward-copy-sexp)

   ;; ("<f11>"     . bookmark-set)
   ;; ("<f11>"     . equake-toggle-fullscreen)
   ;; Move the parenthesis - see SPC k b/B/f/F
   ("M-s-<left>"  . sp-forward-barf-sexp)
   ("M-s-<right>" . sp-forward-slurp-sexp)
   ("C-s-<left>"  . sp-backward-slurp-sexp)
   ("C-s-<right>" . sp-backward-barf-sexp)
;;;;     ("s-;"         . spacemacs/comment-or-uncomment-lines)
   ("S-s-<f1>"    . eshell) ;; Shitf-Super-F1
   ("s-<f1>"      . tw-toggle-shell-pop-multiterm) ;; tw-toggle-shell-pop-term
;;;;     ("s-<f2>"      . projectile-multi-term-in-root)

   ;; M-x term-line-mode - emacs keybindings
   ;; M-x term-char-mode - chars sent directly to terminal
   ("s-<f3>"      . vterm)

   ;; terminal in the current working directory
   ;; ("s-<f1>"      . terminal-here-launch)
;;;;     ;; ("s-<f1>"      . spacemacs/default-pop-shell)
;;;;     ;; ("s-<f1>"      . spacemacs/projectile-shell)
;;;;     ;; jumps to the shell opened by `spacemacs/projectile-shell'
;;;;     ;; ("s-<f1>"      . spacemacs/projectile-shell-pop)
   ;; ("s-<f1>"      . terminal-here-project-launch)
   ("s-W"         . whitespace-cleanup)
   ("s-w"         . tw-whitespace-mode-toggle)
   ("s-m"         . tw-magit-status)
   ("<f3>"   . tw-search-region-or-symbol) ; advice-d
;;;;     ("M-<f3>" . spacemacs/hsearch-project)  ; advice-d

   ("s-a"    . helm-mini) ;; advice-d
   ("s-]"    . helm-mini)
   ;; helm-mini doesn't show all buffers when using layouts (~SPC l~)
;;;;     ("C-s-a"  . spacemacs-layouts/non-restricted-buffer-list-helm) ; advice-d

   ("s-B"    . helm-filtered-bookmarks)
   ("<f9>"   . helm-filtered-bookmarks)
   ;; ("s-p" . helm-projectile)
   ("s-p"    . helm-projectile-find-file)
;;;;     ("s-P"    . spacemacs/helm-persp-switch-project) ; advice-d
   ("s-f"    . find-file)
   ("s-F"    . recentf)
   ;; Can't use `advice'. This is an advice for the binding, not the function
   ("s-r"    . (lambda ()
                 (interactive) (helm-recentf)
                 (message "Use ~s-F~ instead of ~s-r~ for M-x helm-recentf")))
   ("M-y"    . helm-show-kill-ring)   ; replaces evil-paste-pop
   ("s-G"    . helm-google-suggest)
   ("s-/"    . helm-swoop)                          ; advice-d
   ("s-?"    . helm-multi-swoop-all)
;;;;     ("s-l"    . lazy-helm/spacemacs/resume-last-search-buffer)

   ;; TODO crux-duplicate-current-line-or-region gets confused with registry
   ;; content
   ("C-s-<down>" . crux-duplicate-current-line-or-region)
   ("C-s-<up>"   . (lambda (arg) (interactive "p")
                     (crux-duplicate-current-line-or-region arg)
                     (if (evil-normal-state-p)
                         (evil-previous-line)
                       (previous-line))))

   ("C-c d"      . crux-duplicate-current-line-or-region)
   ("C-c t"      . crux-transpose-windows)
   ("C-s-<backspace>" . crux-kill-line-backwards) ; kill-line-backward
   ("s-j"             . crux-top-join-line)

;;;;     ;; TODO xah-backward-block xah-forward-block were removed (by a mistake)
;;;;     ;; in cf18cf842d4097934f58977665925eff004702e2
;;;;     ("<C-up>"            . xah-backward-block)
;;;;     ("<C-down>"          . xah-forward-block)
;;;;     ;; TODO make pg-up / pg-down major-mode specific
;;;;     ;; ("C-<prior>"      . hs-hide-block)    ; pg-up
;;;;     ;; ("C-<next>"       . hs-show-block)    ; pg-down
;;;;     ;; ("C-M-<prior>"    . hs-toggle-hiding) ; pg-up
;;;;     ;; ("C-M-<prior>"    . hs-hide-all)      ; Ctrl + pg-up
;;;;     ;; ("C-M-<next>"     . hs-show-all)      ; Ctrl + pg-down
   ("C-M-<delete>"      . kill-sexp)
   ("C-M-s-<delete>"    . tw-delete-next-sexp)
   ("C-M-s-<backspace>" . tw-delete-prev-sexp)
   ("C-M-<backspace>"   . backward-kill-sexp)

;;;;     ("s-<backspace>"     .
;;;;      ;; Can't use `advice'. This is an advice for the binding, not the function
;;;;      (lambda ()
;;;;        (interactive) (paredit-backward-kill-word)
;;;;        (message "See ~%s~ / M-x "
;;;;                 "SPC k E"
;;;;                 "evil-lisp-state-sp-splice-sexp-killing-backward")))

;;;;     ("s-<delete>"        .
;;;;      ;; Can't use `advice'. This is an advice for the binding, not the function
;;;;      (lambda ()
;;;;        (interactive) (paredit-forward-kill-word)
;;;;        (message "See ~%s~ / M-x "
;;;;                 "SPC k e"
;;;;                 "evil-lisp-state-sp-splice-sexp-killing-forward")))

;;;;     ("M-s-SPC" . spacemacs/evil-search-clear-highlight)
   ("s-g"     . tw-search-or-browse)
   ("s-8" . er/expand-region)     ; increase selected region by semantic units
   ("<f2>"    . evil-avy-goto-char-timer)
   ;; S-<tab> i.e. Shift-Tab i.e. <backtab> calls `next-buffer'

;;;;     ;; TODO s-a when "Last buffer not found."
;;;;     ("s-<tab>" . spacemacs/alternate-buffer)

   ("C-<next>"  . next-buffer)        ; SPC b n; Ctrl-PageDown
   ("s-<right>" . next-buffer)
   ("C-<prior>" . previous-buffer)    ; SPC b p; Ctrl-PageUp
   ("s-<left>"  . previous-buffer)

   ;; same bindings as in the guake terminal
   ("S-s-<up>"    . evil-window-up)
   ("S-s-<down>"  . evil-window-down)
   ("S-s-<left>"  . evil-window-left)
   ("S-s-<right>" . evil-window-right)

;;;;     ;; ("s-<tab>" . popwin:switch-to-last-buffer) ; - for popup buffers??
   ("C-<f2>"  . avy-goto-line) ;; binding clashes with xfce4-workspace
   ("C-s-/"   . avy-goto-line)

   ;; fd - evil-escape from insert state and everything else
   ;; occurences - function scope
   ("s-I"           . tw-iedit-mode-toggle)
   ("s-i"           . iedit-mode)     ; all occurences in the buffer
;;;;     ;; ("s-i"        . spacemacs/enter-ahs-forward)
   ("<f12>"         . undo-tree-visualize)
   ;; ("S-<delete>" . kill-region)
   ("C-s-<delete>"  . kill-line)      ; C-super-key
   ("C-S-<delete>"  . kill-line)      ; C-shift-key
;;;;     ;; ("s-l"        . spacemacs/resume-last-search-buffer)
   ("s-v"           . tw-evil-select-pasted)

   ;; TODO what's the difference between insert and insertchar?
   ("S-s-<insert>" . tw-yank-and-select)

;;;;     ("s-L"   . spacemacs/cycle-line-number-types)
;;;;     ("C-s-l" . spacemacs/cycle-large-file-settings)

   ;; jump like f/t in vim; TODO integrate zop-to-char with 'y' in evil
   ;; zop-up-to-char works as zop-to-char but stop just before target
   ("M-z" . zop-up-to-char)
   ("M-Z" . zop-to-char)

;;;;     ("C-s-." . spacemacs/jump-to-definition-other-window)
;;;;     ("s->"   . spacemacs/jump-to-definition-other-window)

;;;;     ("s-." . spacemacs/jump-to-definition)
   ("s-," . evil-jump-backward)

   ;; ("s-." . evil-goto-definition-imenu)
   ;; ("s-." . evil-goto-definition-xref)
   ;; ("s-," . evil-jump-backward)

   ;; dumb-jump-go has been obsoleted by the xref interface.
   ;; ("s-." . xref-find-definitions) ;; function is part of Emacs
   ;; ("s-," . xref-go-back) ;; function is part of Emacs

   ;; ("s-,"   . cider-pop-back)

   ;; C-o; evil-jump-backward
   ;; C-i; evil-jump-forward; see dotspacemacs-distinguish-gui-tab

   ;; M-x tw-what-face; <C-M-print> on edge doesn't work
   ("<C-print>" . describe-text-properties)

   ("s-<return>"   . jl-jump-last-edited-place)
   ("C-s-<return>" . goto-last-change) ;; M-x evil-goto-last-change ~g ;~
   ;; it's easy to press Shift while holding s-<return> already
   ("S-s-<return>" . evil-jump-backward)

   ("s-J"          . evil-join)

   ("<s-print>" . tw-ediff-buffers-left-right) ; see advice-add
   ("s-A"       . align-regexp)
   ("s-:" . tw-fabricate-subst-cmd) ;; see evil-ex-completion-map bindings

   ("s-<" . tw-select-in-ang-bracket)
   ("s-[" . tw-select-in-sqr-bracket)
   ("s-(" . tw-select-in-rnd-bracket)
   ("s-{" . tw-select-in-crl-bracket)

   ;; may more comfortable than moving the hand away
   ("C-{" . tw-ins-left-paren)
   ("C-}" . tw-ins-right-paren)

   ("s-\"" . tw-select-in-string)

   ("C-S-<mouse-4>" . tw-zoom-all-frames-in)  ;; mouse-up
   ("C-S-<mouse-5>" . tw-zoom-all-frames-out) ;; mouse-down

   ;; Set xfce4-keyboard-settings -> Layout -> Compose key: -
   ;; <menu> is not a prefix key. See:
   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
   ("H-1" . my-H-1) ;; this doesn't work ("C-c h 1" . my-H-1)
   ("H-2" . my-H-2)
   ("H-4" . my-H-4) ;; this doesn't work ("C-c h 4" . my-H-4)
   )
  (message "%s   Note: %s"
           "(my-eval-bind-keys-and-chords) evaluated"
           "~SPC k $~ sp-end-of-sexp")
  )

(my-eval-bind-keys-and-chords)

;; TODO This doesn't work:
;; (with-eval-after-load 'magit-status-mode
;;   (bind-keys :map magit-status-mode-map
;;              ;; TODO use my cycle functionality
;;              ("s-\\" . magit-diff-toggle-refine-hunk)))

;; TODO This doesn't work:
;; (with-eval-after-load 'magit-revision-mode
;;   (bind-keys :map magit-revision-mode-map
;;              ;; TODO use my cycle functionality
;;              ("s-\\" . magit-diff-toggle-refine-hunk)))

(with-eval-after-load 'magit-mode
  (bind-keys :map magit-mode-map
             ;; Workaround for the
             ;; https://github.com/emacs-evil/evil-collection/issues/554
             ("C-v" . evil-visual-line)
             ("1"   . magit-section-show-level-1-all)
             ("2"   . magit-section-show-level-2-all)
             ("3"   . magit-section-show-level-3-all)
             ("4"   . magit-section-show-level-4-all)
             ;; overshadows `(digit-argument <n>)'; use C-M-<n> instead
             ("C-1" . magit-section-show-level-1)
             ("C-2" . magit-section-show-level-2)
             ("C-3" . magit-section-show-level-3)
             ("C-4" . magit-section-show-level-4)))

(defun my-evil-keybindings-in-term (map)
  "Fix evil keybindings in terminals. Alternatively just disable evil-mode:
  (evil-set-initial-state 'term-mode 'emacs)
See also:
- evil-collection-term-setup
- \"TODO: Add support for normal-state editing.\"
https://github.com/emacs-evil/evil-collection/blob/master/modes/term/evil-collection-term.el#L61
"
  (evil-collection-define-key 'insert map (kbd "S-<up>")   #'previous-line)
  (evil-collection-define-key 'insert map (kbd "S-<down>") #'next-line)
  (evil-collection-define-key 'insert map (kbd "C-<up>")   #'previous-line)
  (evil-collection-define-key 'insert map (kbd "C-<down>") #'next-line)
  (evil-collection-define-key 'insert map (kbd "<delete>") #'term-send-del)
  (evil-collection-define-key 'insert map (kbd "<prior>")  #'evil-scroll-page-up)
  (evil-collection-define-key 'insert map (kbd "<next>")   #'evil-scroll-page-down)
;;; simple ~<prior>~, ~<next>~ (i.e. pgup / pgdown) don't even get registered by
;;; Emacs. See: xfconf-query -c xfce4-keyboard-shortcuts -lv | grep Page
  ;; (evil-collection-define-key 'insert map (kbd "s-<prior>") #'evil-scroll-page-up)
  ;; (evil-collection-define-key 'insert map (kbd "s-<next>")  #'evil-scroll-page-down)
  )

;;; (funcall
;;;  (-compose
;;;; 1. `-partial', `apply-partially' and `-compose' accept only functions, not
;;;; macros and `with-eval-after-load' is a macro. Following doesn't work:
;;;;  (-partial #'with-eval-after-load 'term-mode)
;;;; 2. This leads to: Symbol’s value as variable is void: term-raw-map
;;;; `with-eval-after-load' can't be inside a lambda or function. The delayed
;;;; evaluation won't work. `term-raw-map' is defined only after loading
;;;; `multi-term'
;;;   (lambda (body) (with-eval-after-load 'multi-term body)))
;;;
(with-eval-after-load 'multi-term
  ;; term-mode-map is apparently not needed
  (mapcar #'my-evil-keybindings-in-term '(term-raw-map)))

(with-eval-after-load 'dired-mode
  (bind-keys :map dired-mode-map
             ("<f5>"        . tw-revert-buffer-no-confirm)
             ;; ("<f5>"        . revert-buffer)

             ;; Use ~C-s-h~ b/c ~C-H~ (shift-h) doesn't work
             ("C-s-h"       . tw-dired-dotfiles-toggle)
             ("<backspace>" . (lambda () (interactive)
                                (find-alternate-file "..")))
             ;; See https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
             ;; ("<return>"    . dired-find-alternate-file)
             ;; ("<return>"    . diredp-find-file-reuse-dir-buffer)
             ;; asks for file instead of opening it
             ;; ("<return>"    . dired-x-find-file)
             ("<return>"    . dired-find-file) ;; default
             ("<S-delete>"  . tw-dired-do-delete)))

;; (eval-after-load "dired"
;;   '(progn
;;      (defadvice dired-advertised-find-file (around dired-subst-directory
;;                                                    activate)
;;        "Replace current buffer if file is a directory."
;;        (interactive)
;;        (message "%s" #'dired-advertised-find-file)
;;        (let* ((orig (current-buffer))
;;               ;; (filename (dired-get-filename))
;;               (filename (dired-get-filename t t))
;;               (bye-p (file-directory-p filename)))
;;          ad-do-it
;;          (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
;;            (kill-buffer orig))))))

;; (eval-after-load "dired"
;;   ;; don't remove `other-window', the caller expects it to be there
;;   '(defun dired-up-directory (&optional other-window)
;;      "Run Dired on parent directory of current directory."
;;      (interactive "P")
;;      (let* ((dir (dired-current-directory))
;;             (orig (current-buffer))
;;             (up (file-name-directory (directory-file-name dir))))
;;        (or (dired-goto-file (directory-file-name dir))
;;            ;; Only try dired-goto-subdir if buffer has more than one dir.
;;            (and (cdr dired-subdir-alist)
;;                 (dired-goto-subdir up))
;;            (progn
;;              (kill-buffer orig)
;;              (dired up)
;;              (dired-goto-file dir))))))

(with-eval-after-load 'paredit-mode
  (bind-keys :map paredit-mode-map
             ;; these keybindings don't work in the cider-repl-mode-map
             ("<C-right>"    . right-word)
             ("<C-left>"     . left-word)))

(defun my-clj-bind-keys-and-chords (map)
  (bind-keys :map map
             ;; on German keyboard the #-key is next to the Enter-key
             ("C-s-\\" . tw-clj-toggle-reader-comment-current-sexp)
             ("s-\\"   . tw-clj-toggle-reader-comment-fst-sexp-on-line)
             ("s-X"   . tw-switch-to-repl-start-figwheel)
             ("s-e"   . cider-eval-last-sexp)
             ("s-j"   . cider-format-defun)
             ("s-i"   . cljr-rename-symbol))
  (bind-chords :map map ; clojure-mode-map cider-repl-mode-map
               ("pr" . (lambda () (interactive)
                         (tw-insert-str "(println \"\")" 2)))
               ("rm" . (lambda () (interactive)
                         (tw-insert-str "(remove (fn []))" 3)))
               ("fi" . tw-clj-insert-filter-fn)
               ("de" . tw-clj-insert-defn)
               ;; ("db" . my-clj-insert-debugf)
               ;; ("dg" . my-clj-insert-debugf)
               ("df" . tw-clj-insert-fn)
               ("ds" . tw-clj-insert-doseq)
               ("fn" . tw-clj-insert-fn)
               ("do" . tw-clj-insert-do)
               ("co" . tw-clj-insert-comp)
               ("cd" . tw-insert-clojuredocs)
               ("pa" . tw-insert-partial)
               ("le" . tw-clj-insert-let)
               ("fo" . tw-clj-insert-for)
               ("ty" . tw-clj-insert-type)
               ("ma" . tw-clj-insert-map-fn)))

(with-eval-after-load 'cider-repl-mode
  (my-clj-bind-keys-and-chords cider-repl-mode-map)
  (bind-keys :map cider-repl-mode-map
             ("<menu>" . tw-stop-synths-metronoms)
             ("s-h"    . helm-cider-history)
             ("s-j"    . cider-format-defun)
             ("s-x"    . cider-switch-to-last-clojure-buffer)
             ("M-s-l"  . tw-cider-reload-ns-from-file)
             ("s-u"    . tw-cider-reload-ns-from-file)
             ;; invoke from clojure buffer
             ("<C-s-delete>" . cider-repl-clear-buffer)))

(with-eval-after-load 'clojure-mode
  (my-clj-bind-keys-and-chords clojure-mode-map)
  (bind-keys :map clojure-mode-map
             ("s-d"    . cider-eval-defun-at-point)
             ("s-x"    . tw-cider-switch-to-repl-buffer)
             ("C-s-c"  . cider-connect-clj)
             ("C-s-j"  . cider-jack-in)
             ;; ("s-r" . cider-eval-last-expression-in-repl)
             ("M-s-l"  . tw-cider-save-and-load-current-buffer)
             ("s-u"    . tw-cider-save-and-load-current-buffer)
             ("M-s-n"  . cider-repl-set-ns)
             ("s-t"    . cider-test-run-tests)

             ;; TODO see global-map keybindings
             ;; ("s-."  . cider-find-var)
             ;; ("s-,"  . cider-pop-back)
             ;; TODO s-M does not work in REPL buffer

             ;; Reload modified and unloaded namespaces on the classpath
             ("s-o"     . cider-ns-refresh)

             ;; Send a (require ’ns :reload) to the REPL
             ;; ("s-o"  . cider-ns-reload)

             ("C-s-o"   . tw-cider-clear-compilation-highlights)))

(defun endless/sharp ()
  "Insert the function form abbreviation #' unless in a string
or comment.

The reader synstax #' is a function form abbreviation, it enables
byte-compilation, however:
1. Lambdas should never be quoted. I.e. don't do any of this:
      '(lambda (...) ...)
      #'(lambda (...) ...)
2. It doesn't work for `bind-keys' and `bind-chords'
See
https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html"
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(bind-chords :map emacs-lisp-mode-map
             ("df" . tw-elisp-insert-defun)
             ("la" . tw-elisp-insert-lambda)
             ("le" . tw-elisp-insert-let)
             ("me" . tw-elisp-insert-message)
             ("pr" . tw-elisp-insert-message))

(bind-keys :map emacs-lisp-mode-map
           ("C-s-l" . tw-elisp-insert-let)
           ("C-s-m" . tw-elisp-insert-message)
           ("C-s-p" . tw-elisp-insert-message)
           ;; Evaluates the defun above the point. (Is a bit buggy)
           ("C-s-d" . tw-elisp-insert-defun)
           ("s-d"   . eval-defun) ;; tw-eval-current-defun
           ;; The point must be inside the right sexp
           ("M-s-d" . eval-sexp-fu-eval-sexp-inner-list)
           ("#"     . endless/sharp)
           ("s-\\"  . tw-elisp-toggle-reader-comment-current-sexp))

;;;;  (bind-keys :map org-mode-map
;;;;             ;; ~<menu>~ pressed twice
;;;;             ("H-<menu>" . org-latex-export-to-pdf))

;; (with-eval-after-load 'org-mode
;;   (bind-keys :map org-mode-map
;;              ;; ~<menu>~ pressed twice
;;              ("H-<menu>" . org-latex-export-to-pdf)))

(bind-keys :map prog-mode-map
           ;; M-/  M-x hippie-expand
           ("s-Q" . dumb-jump-quick-look)
           ("s-h" . spacemacs/helm-jump-in-buffer)
           ;; previously: helm-imenu-in-all-buffers
           ("s-H" . lazy-helm/helm-imenu-in-all-buffers)
           ("s-u" . eval-buffer)
           ("s-e" . eval-last-sexp))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (bind-keys :map LaTeX-mode-map
                       ("H-<menu>" . latex/build)))) ;; ~<menu>~ pressed twice

;; Setup for Hacking on Guix and Scheme Code
;; https://guix.gnu.org/en/manual/devel/en/guix.html#The-Perfect-Setup
;;
;; `parse-colon-path' returns a list with items containing trailing slash '/',
;; geiser-guile-load-path doesn't like it.
(message "%s glp: %s" context (getenv "glp"))
(when-let ((glp-env (getenv "glp")))
  (let ((glp (split-string glp-env ":"))
        (dgx (getenv "dgx")))
    ;; https://emacs-guix.gitlab.io/website/manual/latest/html_node/Development.html
    (add-hook 'scheme-mode-hook #'guix-devel-mode)

    ;; Put scheme code like e.g utils.scm on the geiser-guile-load-path
    ;; TODO move this to project's .dir-locals.el
    (with-eval-after-load #'geiser-guile
      (mapcar
       ;; Add ELEMENT to the value of LIST-VAR if it isn't there yet.
       (-partial #'add-to-list 'geiser-guile-load-path)
       glp))
    (with-eval-after-load 'yasnippet
      (add-to-list #'yas-snippet-dirs (concat dgx "/etc/snippets/yas")))

    ;; TODO extend the GuixOS with a service providing user full-name and email
    ;; or parse (one of):
    ;;   /run/current-system/configuration.scm
    ;;   `guix system describe | rg "configuration file" | rg -o "/gnu/.*"`

    (setq
     ;; Location for geiser-history.guile and geiser-history.racket. (Default
     ;; "~/.geiser_history")
     ;; geiser-repl-history-filename "..."
     user-full-name         (getenv "user_full_name")
     user-mail-address      (getenv "user_mail_address")
     copyright-names-regexp (format "%s <%s>" user-full-name user-mail-address))

    (load-file (concat dgx "/etc/copyright.el"))
    ;; check if the copyright is up to date M-x copyright-update.
    ;; automatically add copyright after each buffer save
    ;; (add-hook 'after-save-hook 'copyright-update)
    ))

(add-hook 'python-mode-hook
          (lambda ()
            (bind-keys :map python-mode-map
                       ("s-x" . spacemacs/python-start-or-switch-repl))))
(add-hook 'debugger-mode-hook
          (lambda ()
            (bind-keys :map debugger-mode-map
                       ("C-g" . debugger-quit))))

(defun my-fn-kbind-scheme (map)
  (lambda ()
    (bind-keys :map map
               ("C-s-\\" . tw-racket-toggle-reader-comment-current-sexp)
               ("C-s-m"  . tw-scheme-insert-log)
               ("C-s-p"  . tw-scheme-insert-log)
               ("s-."    . geiser-edit-symbol-at-point)
               ;; ("s-;"    . tw-racket-toggle-reader-comment-current-sexp)
               ("s-\\"   . tw-racket-toggle-reader-comment-fst-sexp-on-line)
               ("s-d"    . geiser-eval-definition)
               ("s-e"    . geiser-eval-last-sexp)
               ("s-x"    . geiser-mode-switch-to-repl))
    (bind-chords :map map
                 ("le" . tw-scheme-insert-let*)
                 ("pr" . tw-scheme-insert-log))))

(defun my-fn-kbind-racket (map)
  (lambda ()
    (bind-keys :map map
               ("<C-s-delete>" . my-racket-repl-clear)
               ("C-s-\\" . tw-racket-toggle-reader-comment-current-sexp)
               ("C-s-p"  . tw-racket-insert-log)
               ("M-s-d"  . tw-racket-insert-fn)
               ("M-s-p"  . tw-insert-partial)
               ("s-\\"   . tw-racket-toggle-reader-comment-fst-sexp-on-line)
               ("s-e"    . racket-eval-last-sexp)
               ("s-o"    . racket-run-and-switch-to-repl)
               ("s-x"    . racket-repl))
    (bind-chords :map map
                 ("pr" . tw-racket-insert-log))))

;; (defun set-frame-theme (theme)
;;   "Set the THEME for the current frame only. TODO doesn't work"
;;   (interactive
;;    (list
;;     (intern (completing-read "Theme: " (mapcar 'symbol-name (custom-available-themes))))))
;;   ;; Disable all current themes to avoid mixing theme elements
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (load-theme theme t t) ;; Load the chosen theme without enabling it
;;   ;; Re-enable the previously active themes for other frames
;;   (dolist (other-theme custom-enabled-themes)
;;     (unless (eq other-theme theme)
;;       (enable-theme other-theme)))
;;   ;; Apply the theme only to the current frame - doesn't work!!!
;;   (with-selected-frame (selected-frame)
;;     (enable-theme theme)))
;; (set-frame-theme 'zenburn)

;; (set-background-color "gray0")   ;; black
;; (set-background-color "gray100") ;; white

;; (defun my-haskell-faces ()
;;   "Buffer-local face remapping for `haskell-mode-hook'."
;;   (face-remap-add-relative 'default
;;                            :background "darkgreen"
;;                            :foreground "lightblue"))

;; (add-hook 'haskell-mode-hook #'my-haskell-faces)

;; For rkt-files the bindings are available via major mode bindings.
;; See M-x helm-descbinds
(with-eval-after-load 'racket-mode
  (mapcar (-partial #'apply #'add-hook)
          `((racket-mode-hook      ,(my-fn-kbind-racket racket-mode-map))
            (racket-repl-mode-hook ,(my-fn-kbind-racket racket-repl-mode-map)))))

;; For scm-files the bindings are available via minor mode bindings for
;; geiser-mode, not for scheme-mode. See M-x helm-descbinds
(with-eval-after-load 'geiser-mode
  (mapcar (-partial #'apply #'add-hook)
          `((geiser-mode-hook      ,(my-fn-kbind-scheme geiser-mode-map))
            (geiser-repl-mode-hook ,(my-fn-kbind-scheme geiser-repl-mode-map)))))

(with-eval-after-load 'scheme-mode
  (font-lock-add-keywords
   'scheme-mode (mapcar (lambda (keyword)
                          `(,(concat "(" (regexp-quote keyword) "\\>")
                            . 'font-lock-keyword-face))
                        '("if-let")))
  ;; indentation for 'if-let' should be same as for e.g. 'let'.
  (put 'if-let 'scheme-indent-function 1))

;; advice, defadvice and letf shouldn't be used:
;; https://lists.gnu.org/archive/html/emacs-devel/2012-12/msg00146.html
;; Emacs 24.4 replaces this mechanism with advice-add

;; Difference between `evil-search-forward` and `evil-ex-search-forward`:
;; evil-search-forward    - wrap emacs isearch-forward
;; evil-ex-search-forward - invoke the evil internal search
;; https://emacs.stackexchange.com/a/24913

;; See
;; https://www.reddit.com/r/emacs/comments/6ewd0h/how_can_i_center_the_search_results_vertically/?utm_source=share&utm_medium=web2x

;; (bind-keys :map scheme-mode-map
;;            ("<f11>" . (lambda () (interactive) (forward-sexp 1))))
;; (bind-keys :map scheme-mode-map
;;            ("<f12>" . (lambda () (interactive) (sp-forward-sexp 1))))
;; (unbind-key "<f11>" scheme-mode-map)
;; (unbind-key "<f12>" scheme-mode-map)

(advice-add #'tw-search-region-or-symbol
            :after
            (defun my-note--my-search-region-or-symbol (&optional _)
              (let ((p "[advice tw-search-region-or-symbol] "))
                (message
                 (concat
                  p "Try:\n"
                  p "1. ~<f3>~ then ~<f4>~ then ~v~ (evil-visual-mode)"
                  " mark something and press ~SPC s e~\n"
                  p "2. ~M-<f3>~ for M-x spacemacs/hsearch-project")))))

(advice-add #'split-window-right-and-focus
            :after (defun my-recenter-top-bottom ()
                     ;; needed cause the (recenter-top-bottom) has
                     ;; (interactive "P")
                     (recenter-top-bottom)))
(advice-add #'whitespace-cleanup
            :after (defun my-whitespace-cleanup ()
                     (message "[advice whitespace-cleanup] done")))
(advice-add #'evil-avy-goto-char-timer
            :after (defun my-note--evil-avy-goto-char-timer (_)
                     (message
                      "[advice evil-avy-goto-char-timer] %s"
                      "Also ~SPC j j~, ~<f2>~")))
(advice-add #'evil-avy-goto-line
            :after
            (defun my-note--evil-avy-goto-line (&optional _)
              (message
               "[advice evil-avy-goto-line] %s"
               "Also ~SPC j l~, ~M-m j l~, ~<C-f2>~, ~C-s-/~")))

;; (advice-add #'evil-ex-search-next
;;             :after #'evil-scroll-line-to-center)
;; (advice-add #'evil-ex-search-previous
;;             :after #'evil-scroll-line-to-center)

;; === BEG adjust-point-pos-after-search
(advice-add
 #'evil-ex-search-next
 :before
 #'tw-adjust-point-pos-before-search
 ;; (lambda (&optional COUNT)
 ;;   (interactive)
 ;;   (evil-scroll-line-to-center)
 ;;   ;; (setq my-line-before (line-number-at-pos))
 ;;   )
 ;; ;; convenient name for identifying or removing this advice later
 ;; '((name . "before-search"))
 )
(advice-add #'evil-ex-search-next :after #'tw-adjust-point-pos-after-search)
(advice-add #'evil-ex-search-previous :before #'tw-adjust-point-pos-before-search)
(advice-add #'evil-ex-search-previous :after #'tw-adjust-point-pos-after-search)
(advice-add #'evil-goto-line :before #'evil-scroll-line-to-center)
(advice-add #'evil-goto-line :after #'evil-scroll-line-to-center)

;; Both ~*~ / ~<kp-multiply>~ and ~s-*~ / ~<s-kp-multiply>~ should behave the
;; same and open that transient menu.
(global-set-key (kbd "s-*") #'spacemacs/enter-ahs-backward)
(global-set-key (kbd "<s-kp-multiply>") #'spacemacs/enter-ahs-backward)

;; (advice-remove #'evil-ex-search-next "before-search")
;; (advice-remove #'evil-ex-search-next #'tw-adjust-point-pos-before-search)
;; (advice-remove #'evil-ex-search-next #'tw-adjust-point-pos-after-search)
;; (advice-remove #'evil-ex-search-next #'evil-scroll-line-to-center)
;; (advice-remove #'evil-ex-search-previous #'tw-adjust-point-pos-before-search)
;; (advice-remove #'evil-ex-search-previous #'tw-adjust-point-pos-after-search)
;; (advice-remove #'evil-goto-line #'evil-scroll-line-to-center)
;; (advice-remove #'evil-goto-line #'evil-scroll-line-to-center)

;; === END adjust-point-pos-after-search

(advice-add #'ediff-quit
            :around #'tw-disable-y-or-n-p)
(advice-add #'helm-mini
            :before #'tw-helm-mini)
(advice-add #'helm-mini
            :after
            (defun my-note--evil-avy-goto-char-timer ()
              (message
               "[advice helm-mini] %s"
               "Toggle mark / unmark all buffers: ~M-m~")))
(advice-add #'spacemacs/helm-persp-switch-project
            :after
            (defun my-note--spacemacs/helm-persp-switch-project (_)
              (message
               "[advice spacemacs/helm-persp-switch-project] %s"
               "Try: ~SPC p p~ for M-x helm-projectile-switch-project")))

(advice-add #'spacemacs/toggle-menu-bar
            :after
            (defun my-note--spacemacs/toggle-menu-bar ()
              (message
               "[advice spacemacs/toggle-menu-bar] %s"
               "Try also: ~M-`~ for M-x tmm-menubar")))

(mapcar
 (lambda (map)
   (bind-keys :map map
;;; TODO workaround for (global-set-key (kbd "C-M-k") 'kill-sexp) overridden by
;;; layers/+misc/multiple-cursors/packages.el
              ("C-M-k" . kill-sexp)))
 '(evil-normal-state-map evil-insert-state-map))

;;   (with-eval-after-load 'evil-normal-state
;;     (bind-keys :map evil-normal-state-map
;; ;;; TODO workaround for (global-set-key (kbd "C-M-k") 'kill-sexp) overridden by
;; ;;; layers/+misc/multiple-cursors/packages.el
;;                   ("C-M-k" . kill-sexp)))

;;   (with-eval-after-load 'evil-insert-state
;;     (bind-keys :map evil-insert-state-map
;; ;;; TODO workaround for (global-set-key (kbd "C-M-k") 'kill-sexp) overridden by
;; ;;; layers/+misc/multiple-cursors/packages.el
;;                   ("C-M-k" . kill-sexp)))

(mapcar
 (lambda (map)
   ;; Move by screen lines instead of logical (long) lines
   (bind-keys :map map
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)))
 '(evil-motion-state-map evil-visual-state-map))

(bind-keys :map evil-visual-state-map
           ("p" . tw-evil-paste-after-from-0))

;; (with-eval-after-load 'evil-motion-state
;;    ;; Move by screen lines instead of logical (long) lines
;;    (bind-keys :map evil-motion-state-map
;;               ("j" . evil-next-visual-line)
;;               ("k" . evil-previous-visual-line)))

;; (with-eval-after-load 'evil-visual-state
;;   ;; Move by screen lines instead of logical (long) lines
;;   (bind-keys :map evil-visual-state-map
;;                   ("j" . evil-next-visual-line)
;;                   ("k" . evil-previous-visual-line))
;;   (bind-keys :map evil-visual-state-map
;;              ("p" . tw-evil-paste-after-from-0)))

;; see also binding for <f2>
;; (bind-keys :map evil-normal-state-map
;;            ("f" . evil-avy-goto-char-timer)
;;            ("t" . evil-avy-goto-char-timer))

;; (add-to-list 'spacemacs-indent-sensitive-modes #'clojure-mode)
;; (add-to-list 'spacemacs-indent-sensitive-modes 'clojurescript-mode)

;; accept completion from copilot and fallback to company

;;; github copilot config begin
;; (with-eval-after-load 'company
;;   ;; disable inline previews
;;   (delq 'company-preview-if-just-one-frontend company-frontends))

;; (with-eval-after-load 'copilot
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; (add-hook 'prog-mode-hook 'copilot-mode)

;; (define-key evil-insert-state-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)
;; (define-key evil-insert-state-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
;;; github copilot config end

;; Max column width of buffer names before truncate. (Default 20)
(setq helm-buffer-max-length nil)

;; Truncate lines in ‘helm-buffers-list’ when non-nil. (Default t)
;; helm-buffer-max-length must be nil in order to have the buffer
;; names not truncated
(setq helm-buffers-truncate-lines nil)

;; 1. Either turn off creation of backups entirely:
(setq make-backup-files nil)
;; 2. Or alternatively move the backups away
;;   Emacs.org~
;;   #Emacs.org#
;;   .#Emacs.org
;;   ~/.emacs.d/.lsp-session-v1
;;   ~/.emacs.d/transient/
;;   ~/.emacs.d/projectile-bookmarks.eld
;; (setq
;;  backup-directory-alist
;;  `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))


;; Don't create the lock file like .#Emacs.org. They appear when you
;; have unsaved changes to a file in a buffer!
;; Unfortunately these can’t be moved, but they can be disabled:
(setq create-lockfiles nil)

(beacon-mode)

;; pulse modified regions
;; https://github.com/minad/goggles

;; alternative to copy-sexp.el
;; TODO (pulse-momentary-highlight-region ...) ;; built-in

;; For collaborative editing we need to setup a crdt sever at first!
;; The docs is not clear about that.

;; abcdw, Hello Andrew, I'd like to ask you how do you develop your emacs config in the rde project?
;; I guess there's some impedancy between writing scheme code for emacs (i.e. for elisp code) so then when you want to try out the changes you need the evaluate the scheme code and that takes time, right?
;; So ist there a way for to speed up that part of the development process?

;; https://protesilaos.com/codelog/2023-12-18-emacs-org-advanced-literate-conf/
(defun spacemacs-toggle-gui-elements (&optional on-off)
  "Toggle menu bar, tool bar, scroll bars, and tool tip modes. If
optional ON-OFF is not specified, then toggle on/off state. If
ON-OFF is 0 or 1, then turn gui elements OFF or ON respectively."
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode (or on-off (not scroll-bar-mode))))
  (when  (fboundp 'tool-bar-mode)
    (tool-bar-mode (or on-off (not tool-bar-mode))))
  (unless (memq (window-system) '(mac ns))
    (when (fboundp 'menu-bar-mode)
      (menu-bar-mode (or on-off (not menu-bar-mode)))))
  ;; tooltips in echo-aera
  (when (fboundp 'tooltip-mode)
    (tooltip-mode (or on-off (not tooltip-mode))))
  ;; Doesn't work
  ;; (toggle-frame-fullscreen)
  )

(spacemacs-toggle-gui-elements 0)

;; Many files are created relative to user-emacs-directory already! We
;; can change it in our config:
(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

;; activate origami-mode so that folding ~z a~ works
(origami-mode)

(setq
 erc-fill-column 120
 ;; erc-fill-function 'erc-fill-variable
 erc-fill-function 'erc-fill-static
 erc-fill-static-center 15
 ;; erc-enable-notifications nil
 erc-autojoin-channels-alist
 '(("libera.chat" "#guix"
    ;; "#systemcrafters"
    ))
 erc-prompt-for-nickserv-password nil
 erc-server-list
 (list (list "irc.libera.chat" :port "6667"
             :nick (getenv "IRC_USER")
             :password (getenv "IRC_PASSWD"))))
