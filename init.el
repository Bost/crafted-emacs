(setq-local context "init:") ;; for log messages

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(load "~/.emacs.d.distros/crafted-emacs/modules/crafted-init-config")

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
   tweaks
   ;; jump-last
   ))

(dolist (package modules-and-packages)
  ;; message does the prettyfing, we need to compose `print after format` functions
  (print (format "%s (require '%s) ..." context package))
  (require package))
;; (message "%s all requires done" context)

(crafted-package-install-package 'doom-themes)
(progn
  (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
  ;; (load-theme 'doom-palenight t)     ; load the doom-palenight theme
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-solarized-light t)
  )

(defun spacemacs/mplist-remove (plist prop)
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
  "Set the font given the passed PLISTS.

PLISTS has either the form (\"fontname\" :prop1 val1 :prop2 val2 ...)
or is a list of such. The first font that can be found will be used.

The return value is nil if no font was found, truthy otherwise."
  (unless (listp (car plists))
    (setq plists (list plists)))
  (catch 'break
    (dolist (plist plists)
      (message "################ %s (font-spec :name (car plist)) %s" context (font-spec :name (car plist)))
      (message "################ %s (find-font (font-spec :name (car plist))) %s" context (find-font (font-spec :name (car plist))))
      (when (find-font (font-spec :name (car plist)))
        (message "################ %s find-font" context)
        (let* ((font (car plist))
               (props (cdr plist))
               ;; TODO there's a bug in spacemacs/mplist-remove
               (font-props (spacemacs/mplist-remove props :powerline-offset))
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
          (let* ((fallback-props (spacemacs/mplist-remove
                                  (spacemacs/mplist-remove font-props :size)
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
            ;; mode-line additional characters (circled/squared mathematical operators)
            (set-fontset-font "fontset-default"
                              '(#x2295 . #x22a1) fallback-spec nil 'prepend)
            ;; new version lighter (arrow block)
            (set-fontset-font "fontset-default"
                              '(#x2190 . #x21ff) fallback-spec nil 'prepend)))
        (message "################ %s font set" context)
        (throw 'break t)))
    (progn
      (message "################ %s find-font: false" context)
      nil)))

;; (window-parameter (selected-window) 'no-delete-other-windows)

(message "%s set-default-font ..." context)
(setq default-font
      `("Source Code Pro"
        :size
        ,(let* ((hostname (system-name))
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
           point-size)
        :weight normal
        :width normal))

(set-default-font default-font)

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
;; (set-default-font-prot)

(message "%s set-default-font ... done" context)

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
(defun spacemacs//toggle-gui-elements (&optional on-off)
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
    (tooltip-mode (or on-off (not tooltip-mode)))))

(spacemacs//toggle-gui-elements 0)

(bind-keys ; :map global-map
     :map global-map
;;;;      ("<f5>" . my=revert-buffer-no-confirm)
      ;; ("s-*"    . er/contract-region) ;; TODO see https://github.com/joshwnj
 
      ;; TODO The <escape> keybinding seems not to work.
      ;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
      ;; TODO notmuch
 
      ;; TODO my=emacs-comment-sexp: mark-sexp C-M-@ comment-dwim M-;
 
      ;; the funny keys can be seen
      ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2008-11/msg00011.html
      ("C-s-<268632070>" . my=H-3) ;; this is probably for an Apple computers
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
;;;;      ("s-R"       . spacemacs/rename-current-buffer-file)
;;;;      ("s-q"       . my=other-window)
      ("s-k"       . kb-close-buffer)
      ("s-s"       . save-buffer)
;;;;      ("s-0"       . my=delete-window)
;;;;      ("s-1"       . my=delete-other-windows)
      ("S-s-<f8>"    . ace-swap-window)
;;;;      ;; ("S-s-<f8>" . transpose-frame)
;;;;      ("s-N"       . spacemacs/cycle-defun-narrow-modes)
;;;;      ("s-n"       . spacemacs/cycle-narrow-widen)
;;;;      ;; ("s-2"    . my=split-other-window-below)
      ("s-2"       . split-window-below) ; ~SPC w -~
;;;;      ;; see ~SPC w /~ and ~SPC w 2~
;;;;      ;; ("s-3"    . spacemacs/window-split-double-columns)
;;;;      ;; see ~SPC w /~ and ~SPC w 2~
      ("s-3"       . split-window-right-and-focus)
;;;;      ("s-9"       . my=load-layout)
;;;;      ("s-+"       . my=eval-bind-keys-and-chords)
;;;;      ("s-<kp-add>". my=eval-bind-keys-and-chords)
;;;;      ("s-z"       . my=buffer-selection-show)
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
;;;;      ("s-;"         . spacemacs/comment-or-uncomment-lines)
      ("S-s-<f1>"    . eshell) ;; Shitf-Super-F1
;;;;      ("s-<f1>"      . my=toggle-shell-pop-multiterm) ;; my=toggle-shell-pop-term
;;;;      ("s-<f2>"      . projectile-multi-term-in-root)
 
      ;; M-x term-line-mode - emacs keybindings
      ;; M-x term-char-mode - chars sent directly to terminal
      ("s-<f3>"      . vterm)
 
      ;; terminal in the current working directory
      ;; ("s-<f1>"      . terminal-here-launch)
;;;;      ;; ("s-<f1>"      . spacemacs/default-pop-shell)
;;;;      ;; ("s-<f1>"      . spacemacs/projectile-shell)
;;;;      ;; jumps to the shell opened by `spacemacs/projectile-shell'
;;;;      ;; ("s-<f1>"      . spacemacs/projectile-shell-pop)
      ;; ("s-<f1>"      . terminal-here-project-launch)
      ("s-W"         . whitespace-cleanup)
;;;;      ("s-w"         . my=whitespace-mode-toggle)
;;;;      ("s-m"         . my=magit-status)
;;;;      ("<f3>"   . my=search-region-or-symbol) ; advice-d
;;;;      ("M-<f3>" . spacemacs/hsearch-project)  ; advice-d
 
;;;;      ("s-a"    . helm-mini) ;; advice-d
;;;;      ("s-]"    . helm-mini)
;;;;      ;; helm-mini doesn't show all buffers when using layouts (~SPC l~)
;;;;      ("C-s-a"  . spacemacs-layouts/non-restricted-buffer-list-helm) ; advice-d
 
;;;;      ("s-B"    . helm-filtered-bookmarks)
;;;;      ("<f9>"   . helm-filtered-bookmarks)
;;;;      ;; ("s-p" . helm-projectile)
;;;;      ("s-p"    . helm-projectile-find-file)
;;;;      ("s-P"    . spacemacs/helm-persp-switch-project) ; advice-d
;;;;      ("s-f"    . helm-find-files)
;;;;      ("s-F"    . helm-recentf)
;;;;      ;; Can't use `advice'. This is an advice for the binding, not the function
;;;;      ("s-r"    . (lambda ()
;;;;                    (interactive) (helm-recentf)
;;;;                    (message "Use ~s-F~ instead of ~s-r~ for M-x helm-recentf")))
;;;;      ("M-y"    . helm-show-kill-ring)   ; replaces evil-paste-pop
;;;;      ("s-G"    . helm-google-suggest)
;;;;      ("s-/"    . helm-swoop)                          ; advice-d
;;;;      ("s-?"    . helm-multi-swoop-all)
;;;;      ("s-l"    . lazy-helm/spacemacs/resume-last-search-buffer)
 
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
 
;;;;      ;; TODO xah-backward-block xah-forward-block were removed (by a mistake)
;;;;      ;; in cf18cf842d4097934f58977665925eff004702e2
;;;;      ("<C-up>"            . xah-backward-block)
;;;;      ("<C-down>"          . xah-forward-block)
;;;;      ;; TODO make pg-up / pg-down major-mode specific
;;;;      ;; ("C-<prior>"      . hs-hide-block)    ; pg-up
;;;;      ;; ("C-<next>"       . hs-show-block)    ; pg-down
;;;;      ;; ("C-M-<prior>"    . hs-toggle-hiding) ; pg-up
;;;;      ;; ("C-M-<prior>"    . hs-hide-all)      ; Ctrl + pg-up
;;;;      ;; ("C-M-<next>"     . hs-show-all)      ; Ctrl + pg-down
      ("C-M-<delete>"      . kill-sexp)
;;;;      ("C-M-s-<delete>"    . my=delete-next-sexp)
;;;;      ("C-M-s-<backspace>" . my=delete-prev-sexp)
      ("C-M-<backspace>"   . backward-kill-sexp)
 
;;;;      ("s-<backspace>"     .
;;;;       ;; Can't use `advice'. This is an advice for the binding, not the function
;;;;       (lambda ()
;;;;         (interactive) (paredit-backward-kill-word)
;;;;         (message "See ~%s~ / M-x "
;;;;                  "SPC k E"
;;;;                  "evil-lisp-state-sp-splice-sexp-killing-backward")))
 
;;;;      ("s-<delete>"        .
;;;;       ;; Can't use `advice'. This is an advice for the binding, not the function
;;;;       (lambda ()
;;;;         (interactive) (paredit-forward-kill-word)
;;;;         (message "See ~%s~ / M-x "
;;;;                  "SPC k e"
;;;;                  "evil-lisp-state-sp-splice-sexp-killing-forward")))
 
;;;;      ("M-s-SPC" . spacemacs/evil-search-clear-highlight)
;;;;      ("s-g"     . my=search-or-browse)
      ("s-8" . er/expand-region)     ; increase selected region by semantic units
      ("<f2>"    . evil-avy-goto-char-timer)
      ;; S-<tab> i.e. Shift-Tab i.e. <backtab> calls `next-buffer'
 
;;;;      ;; TODO s-a when "Last buffer not found."
;;;;      ("s-<tab>" . spacemacs/alternate-buffer)
 
      ("C-<next>"  . next-buffer)        ; SPC b n; Ctrl-PageDown
      ("s-<right>" . next-buffer)
      ("C-<prior>" . previous-buffer)    ; SPC b p; Ctrl-PageUp
      ("s-<left>"  . previous-buffer)
 
      ;; same bindings as in the guake terminal
      ("S-s-<up>"    . evil-window-up)
      ("S-s-<down>"  . evil-window-down)
      ("S-s-<left>"  . evil-window-left)
      ("S-s-<right>" . evil-window-right)
 
;;;;      ;; ("s-<tab>" . popwin:switch-to-last-buffer) ; - for popup buffers??
      ("C-<f2>"  . avy-goto-line) ;; binding clashes with xfce4-workspace
      ("C-s-/"   . avy-goto-line)
 
;;;;      ;; fd - evil-escape from insert state and everything else
;;;;      ;; occurences - function scope
;;;;      ("s-I"           . my=iedit-mode-toggle)
      ("s-i"           . iedit-mode)     ; all occurences in the buffer
;;;;      ;; ("s-i"        . spacemacs/enter-ahs-forward)
      ("<f12>"         . undo-tree-visualize)
      ;; ("S-<delete>" . kill-region)
      ("C-s-<delete>"  . kill-line)      ; C-super-key
      ("C-S-<delete>"  . kill-line)      ; C-shift-key
;;;;      ;; ("s-l"        . spacemacs/resume-last-search-buffer)
;;;;      ("s-v"           . my=evil-select-pasted)
 
;;;;      ;; TODO what's the difference between insert and insertchar?
;;;;      ("S-s-<insert>" . my=yank-and-select)
 
;;;;      ("s-L"   . spacemacs/cycle-line-number-types)
;;;;      ("C-s-l" . spacemacs/cycle-large-file-settings)
 
      ;; jump like f/t in vim; TODO integrate zop-to-char with 'y' in evil
      ;; zop-up-to-char works as zop-to-char but stop just before target
      ("M-z" . zop-up-to-char)
      ("M-Z" . zop-to-char)
 
;;;;      ("C-s-." . spacemacs/jump-to-definition-other-window)
;;;;      ("s->"   . spacemacs/jump-to-definition-other-window)
 
;;;;      ("s-." . spacemacs/jump-to-definition)
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
 
      ;; M-x my=what-face; <C-M-print> on edge doesn't work
      ("<C-print>" . describe-text-properties)
 
      ("s-<return>"   . jl-jump-last-edited-place)
      ("C-s-<return>" . goto-last-change) ;; M-x evil-goto-last-change ~g ;~
      ;; it's easy to press Shift while holding s-<return> already
      ("S-s-<return>" . evil-jump-backward)
 
      ("s-J"          . evil-join)
 
;;;;      ("<s-print>" . my=ediff-buffers-left-right) ; see advice-add
      ("s-A"       . align-regexp)
;;;;      ("s-:" . my=fabricate-subst-cmd) ;; see evil-ex-completion-map bindings
 
;;;;      ("s-<" . my=select-in-ang-bracket)
;;;;      ("s-[" . my=select-in-sqr-bracket)
;;;;      ("s-(" . my=select-in-rnd-bracket)
;;;;      ("s-{" . my=select-in-crl-bracket)
 
;;;;      ;; may more comfortable than moving the hand away
;;;;      ("C-{" . my=ins-left-paren)
;;;;      ("C-}" . my=ins-right-paren)
 
;;;;      ("s-\"" . my=select-in-string)
 
;;;;      ("C-S-<mouse-4>" . my=zoom-all-frames-in)  ;; mouse-up
;;;;      ("C-S-<mouse-5>" . my=zoom-all-frames-out) ;; mouse-down
 
;;;;      ;; Set xfce4-keyboard-settings -> Layout -> Compose key: -
;;;;      ;; <menu> is not a prefix key. See:
;;;;      ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Prefix-Keys.html
;;;;      ("H-1" . my=H-1) ;; this doesn't work ("C-c h 1" . my=H-1)
;;;;      ("H-2" . my=H-2)
;;;;      ("H-4" . my=H-4) ;; this doesn't work ("C-c h 4" . my=H-4)
     )
