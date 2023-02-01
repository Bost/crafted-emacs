;;; crafted-evil-config.el --- Evil mode configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Evil mode configuration, for those who prefer `Vim' keybindings.

;;; Code:

;; Turn on undo-tree globally for version older than 28.  Use
;; undo-redo for Emacs 28+
(when (and (< emacs-major-version 28)
           (featurep 'undo-tree))
  (global-undo-tree-mode))

;; Set some variables that must be configured before loading the package
(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
;; C-h is backspace in insert state
(customize-set-variable 'evil-want-C-h-delete t)
(if (< emacs-major-version 28)
  (customize-set-variable 'evil-undo-system 'undo-tree)
  (customize-set-variable 'evil-undo-system 'undo-redo))

(defun crafted-evil-vim-muscle-memory ()
  "Make a more familiar Vim experience.

Take some of the default keybindings for evil mode."
    (customize-set-variable 'evil-want-C-i-jump t)
    (customize-set-variable 'evil-want-Y-yank-to-eol t)
    (customize-set-variable 'evil-want-fine-undo t))

;; Load Evil and enable it globally
(require 'evil)
(evil-mode 1)

;; Make evil search more like vim
(evil-select-search-module 'evil-search-module 'evil-search)

;; Turn on Evil Nerd Commenter
(evilnc-default-hotkeys)

;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(defun crafted-evil-discourage-arrow-keys ()
  "Turn on a message to discourage use of arrow keys.

Rebinds the arrow keys to display a message instead."

  (defun crafted-evil/discourage-arrow-keys ()
    (message "Use HJKL keys instead!"))

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>")  'crafted-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<right>") 'crafted-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<down>")  'crafted-evil/discourage-arrow-keys)
  (define-key evil-normal-state-map (kbd "<up>")    'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<left>")  'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<right>") 'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<down>")  'crafted-evil/discourage-arrow-keys)
  (evil-global-set-key 'motion      (kbd "<up>")    'crafted-evil/discourage-arrow-keys))

;; Make sure some modes start in Emacs state
;; TODO: Split this out to other configuration modules?
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(evil-collection-init)

(provide 'crafted-evil-config)
;;; crafted-evil-config.el ends here