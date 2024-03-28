;;; spacemacs-auto-highlight.el --- From Spacemacs -*- lexical-binding: t; -*-

(require 'auto-highlight-symbol)

(defun spacemacs/goto-last-searched-ahs-symbol ()
  "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
  (interactive)
  (if (bound-and-true-p spacemacs-last-ahs-highlight-p)
      (progn (goto-char (nth 1 spacemacs-last-ahs-highlight-p))
             (spacemacs//ahs-setup)
             (spacemacs/symbol-highlight-transient-state/body))
    (message "No previously searched for symbol found")))

(defun spacemacs/integrate-evil-search (forward)
        ;; isearch-string is last searched item.  Next time
        ;; "n" is hit we will use this.
        (let* ((symbol (evil-find-thing forward 'symbol))
               (regexp (concat "\\<" symbol "\\>")))
          (setq isearch-string regexp
                isearch-regexp regexp
                evil-ex-search-pattern (evil-ex-make-search-pattern regexp)))
        ;; Next time "n" is hit, go the correct direction.
        (setq isearch-forward forward)
        (setq evil-ex-search-direction (if forward 'forward 'backward))
        ;; ahs does a case sensitive search.  We could set
        ;; this, but it would break the user's current
        ;; sensitivity settings.  We could save the setting,
        ;; then next time the user starts a search we could
        ;; restore the setting.
        ;;(setq case-fold-search nil)
        ;; Place the search term into the search rings.
        (isearch-update-ring isearch-string t)
        (evil-push-search-history isearch-string forward)
        ;; Use this search term for empty pattern "%s//replacement/"
        ;; Append case sensitivity
        (setq evil-ex-last-was-search nil
              evil-ex-substitute-pattern `(,(concat isearch-string "\\C")
                                           nil (0 0))))

(defun spacemacs//ahs-setup ()
  "Remember the `auto-highlight-symbol-mode' state,
before highlighting a symbol."
  (unless (bound-and-true-p auto-highlight-symbol-mode)
    (setq-local spacemacs//ahs-was-disabled t))
  (auto-highlight-symbol-mode)
  (ahs-highlight-now))

(defun spacemacs/enter-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq spacemacs--ahs-searching-forward t)
  (spacemacs/quick-ahs-forward))

(defun spacemacs/enter-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (setq spacemacs--ahs-searching-forward nil)
  (spacemacs/quick-ahs-forward))

(defun spacemacs/quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (spacemacs//quick-ahs-move t))

(defun spacemacs/quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with
 `auto-highlight-symbol'"
  (interactive)
  (spacemacs//quick-ahs-move nil))

(defun spacemacs//quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with
 `auto-highlight-symbol'"
  (evil-set-jump)
  (spacemacs//ahs-setup)
  (if (eq forward spacemacs--ahs-searching-forward)
      (progn
        (spacemacs/integrate-evil-search t)
        (ahs-forward))
    (spacemacs/integrate-evil-search nil)
    (ahs-backward))
  (spacemacs/symbol-highlight-transient-state/body))

(defun spacemacs/symbol-highlight ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (spacemacs/integrate-evil-search t)
  (spacemacs//remember-last-ahs-highlight)
  (spacemacs//ahs-setup)
  (spacemacs/symbol-highlight-transient-state/body))

(defun spacemacs//remember-last-ahs-highlight ()
  (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p)))

(defvar-local spacemacs//ahs-was-disabled t
  "This is used to disable `auto-highlight-symbol-mode',
when the Symbol Highlight Transient State is closed.
If ahs mode was disabled before a symbol was highlighted.")

(defun spacemacs//ahs-was-disabled-in-ahs-ts-exit-window-p ()
  (let ((prev-win (selected-window)))
    (select-window spacemacs//ahs-ts-exit-window)
    (prog1 spacemacs//ahs-was-disabled
      (select-window prev-win))))

(defvar spacemacs//ahs-ts-exit-window nil
  "Remember the selected window when the
Symbol Highlight Transient State is closed.

This is used to disable `auto-highlight-symbol-mode',
in the window where the Symbol Highlight Transient State was closed,
when the TS was closed by opening a prompt. For example:
 SPC SPC (or M-x)       ;; spacemacs/helm-M-x-fuzzy-matching
 SPC ?                  ;; helm-descbinds
 M-:                    ;; eval-expression
 :                      ;; evil-ex

ahs mode is only disabled if it was disabled before a symbol was highlighted.")

(defun spacemacs//ahs-ts-on-exit ()
  (setq spacemacs//ahs-ts-exit-window (selected-window))
  ;; Restore user search direction state as ahs has exitted in a state
  ;; good for <C-s>, but not for 'n' and 'N'"
  (setq isearch-forward spacemacs--ahs-searching-forward)
  (spacemacs//disable-symbol-highlight-after-ahs-ts-exit))

(defun spacemacs//disable-symbol-highlight-after-ahs-ts-exit ()
  "Disable `auto-highlight-symbol-mode', when the
Symbol Highlight Transient State buffer isn't found.
This occurs when the TS wasn't restarted.
It is restarted when navigating to the next or previous symbol.

ahs mode is only disabled if it was disabled before a symbol was highlighted."
  (run-with-idle-timer
   0 nil
   (lambda ()
     (unless (string= (spacemacs//transient-state-buffer-title)
                      "Symbol Highlight")
       (cond ((and (spacemacs//prompt-opened-from-ahs-ts-p)
                   (spacemacs//ahs-was-disabled-in-ahs-ts-exit-window-p))
              (spacemacs//disable-ahs-mode-in-ahs-ts-exit-window))
             (spacemacs//ahs-was-disabled
              (spacemacs//disable-symbol-highlight)))))))

(defun spacemacs//prompt-opened-from-ahs-ts-p ()
  "Was a prompt opened (for example: M-x),
from the Symbol Highlight Transient State?"
  (not (eq spacemacs//ahs-ts-exit-window (selected-window))))

(defun spacemacs//disable-ahs-mode-in-ahs-ts-exit-window ()
  "Disable `auto-highlight-symbol-mode',
in the window where the Symbol Highlight Transient State was closed."
  (let ((prev-win (selected-window)))
    (select-window spacemacs//ahs-ts-exit-window)
    (spacemacs//disable-symbol-highlight)
    (setq spacemacs//ahs-ts-exit-window nil)
    (select-window prev-win)))

(defun spacemacs//disable-symbol-highlight ()
  (auto-highlight-symbol-mode -1)
  (setq-local spacemacs//ahs-was-disabled nil))

(defun spacemacs//transient-state-buffer-title ()
  (let ((transient-state-buffer-name " *LV*"))
    (when (get-buffer transient-state-buffer-name)
      (with-current-buffer transient-state-buffer-name
        (buffer-substring-no-properties
         (point-min)
         (string-match "Transient State" (buffer-string)))))))

(defun spacemacs/symbol-highlight-reset-range ()
  "Reset the range for `auto-highlight-symbol'."
  (interactive)
  (ahs-change-range ahs-default-range))

;; transient state
(defun spacemacs//symbol-highlight-doc ()
        (let* ((i 0)
               (overlay-list (ahs-overlay-list-window))
               (overlay-count (length overlay-list))
               (overlay (format "%s" (nth i overlay-list)))
               (current-overlay (format "%s" (ahs-current-overlay-window)))
               (st (ahs-stat))
               (plighter (ahs-current-plugin-prop 'lighter))
               (plugin (format "%s"
                               (cond ((string= plighter "HS")  "Display")
                                     ((string= plighter "HSA") "Buffer")
                                     ((string= plighter "HSD") "Function"))))
               (face (cond ((string= plighter "HS")  ahs-plugin-default-face)
                           ((string= plighter "HSA") ahs-plugin-whole-buffer-face)
                           ((string= plighter "HSD") ahs-plugin-bod-face))))
          (while (not (string= overlay current-overlay))
            (setq i (1+ i))
            (setq overlay (format "%s" (nth i overlay-list))))
          (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                 (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" "")))
            (concat
             (propertize (format " %s " plugin) 'face face)
             (propertize (format " %s%s " x/y hidden) 'face
                         `(:foreground "#ffffff" :background "#000000"))))))

(defun spacemacs/ahs-to-iedit ()
  "Trigger iedit from ahs."
  (interactive)
  (cond
   ((and (not (eq dotspacemacs-editing-style 'emacs))
         (configuration-layer/package-used-p 'evil-iedit-state))
    (evil-iedit-state/iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   ((and (eq dotspacemacs-editing-style 'emacs)
         (configuration-layer/package-used-p 'iedit))
    (iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (t (ahs-edit-mode t))))

(defun spacemacs//symbol-highlight-ts-doc ()
  (spacemacs//transient-state-make-doc
   'symbol-highlight
   (format spacemacs--symbol-highlight-transient-state-doc
           (spacemacs//symbol-highlight-doc))))

(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "*")
              'spacemacs/enter-ahs-forward)
  (define-key evil-motion-state-map (kbd "#")
              'spacemacs/enter-ahs-backward))

(provide 'spacemacs-auto-highlight)
