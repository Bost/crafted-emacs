;;; terminals.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Rostislav Svoboda

;; Author: Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;; Keywords:

(require 'shell)
(require 'shell-pop)
(require 'term)
;; (require 'vterm)
(require 'multi-term)
;; (require 'multi-vterm)

(defun multiterm (&optional ARG)
  "Wrapper to be able to call multi-term from shell-pop"
  (interactive)
  (multi-term))

;; (defun multivterm (&optional ARG)
;;   "Wrapper to be able to call multi-vterm from shell-pop"
;;   (interactive)
;;   (multi-vterm))

(defvar shell-default-width 30
  "Width in percents for the shell window.")

(setq shell-pop-window-position 'right
      shell-pop-window-size     30
      shell-pop-term-shell      shell-file-name
      shell-pop-full-span       t)

(defun spacemacs/resize-shell-to-desired-width ()
  (when (and (string= (buffer-name) shell-pop-last-shell-buffer-name)
             (memq shell-pop-window-position '(left right)))
    (enlarge-window-horizontally (- (/ (* (frame-width) shell-default-width)
                                       100)
                                    (window-width)))))

(defun spacemacs/shell-pop-multiterm (index)
  "Toggle a popup window with `multiterm'.

Multiple shells can be opened with a numerical prefix
argument. Using the universal prefix argument will open the shell
in the current buffer instead of a popup buffer."
  (interactive "P")
  (if (equal '(4) index)
      (multiterm nil)
    (shell-pop--set-shell-type
     'shell-pop-shell-type
     (list "multiterm"
           (concat "*Default-"
                   (if (file-remote-p default-directory)
                       "remote-" "")
                   "multiterm" "*")
           (lambda nil
             (multiterm nil))))
    (shell-pop index)
    (spacemacs/resize-shell-to-desired-width)))

(provide 'terminals)
