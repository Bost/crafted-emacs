;;; crafted-early-init-config.el --- Crafted early initialization  -*- lexical-binding: t; -*-

;; Copyright (C) 2023
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; Code to setup `package.el' during `early-init.el'

;;; Code:

(setq-local context "crafted-package-config:") ;; for log messages
(require 'package)

;;; Setup Emacs Lisp Package Archives (ELPAs)
;; where to get packages to install
(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; It seems that this macro cannot be added to emacs-tweaks, as it only gets
;; loaded during a later stage of Spacemacs initialization.
(defmacro my=def-evar (elisp-var def-val evar-name)
  "Define an Emacs variable from environment with defaults. Warn if
differences were encountered."
  `(let* ((evar-val (or (getenv ,evar-name) ,def-val)))
     (setq ,elisp-var (or (getenv ,evar-name) ,def-val))
     (unless (string= ,elisp-var (expand-file-name ,def-val))
       (message "WARN (expand-file-name def-val): %s and evar: %s=%s differ"
                (expand-file-name ,def-val) ,evar-name evar-val))))

(my=def-evar dev  "~/dev"          "dev")
(my=def-evar dotf "~/dev/dotfiles" "dotf")

(customize-set-variable 'package-archives
 ;; Default values
 ;; `(("melpa"  . "https://melpa.org/packages/")
 ;;   ("stable" . "https://stable.melpa.org/packages/")
 ;;   ("gnu"    . "https://elpa.gnu.org/packages/")
 ;;   ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
 ;; Local mirrors created by https://github.com/d12frosted/elpa-mirror
 ;; See also:
 ;;   https://github.com/redguardtoo/elpa-mirror
 ;;   https://github.com/melpa/melpa
 ;; Update by running (fish-shell):
 ;;   set mirror $dev/elpa-mirror.d12frosted
 ;;   git --git-dir=$mirror/.git --work-tree=$mirror pull --rebase
 (let ((mirror (concat dev "/elpa-mirror.d12frosted")))
   `(("melpa"  . ,(concat mirror "/melpa/"))
     ("gnu"    . ,(concat mirror "/gnu/"))
     ("nongnu" . ,(concat mirror "/nongnu/"))
     ("stable" . ,(concat mirror "/stable-melpa/"))
     )))
(message "%s package-archives \n%s" context (pp package-archives))

;;; Configure ELPA priorities
;; Prefer GNU sources and stable versions before development versions from MELPA.
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                        ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                        ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                        ; from melpa

;;; Forms to refresh package archive contents
;; These functions are available to use for deciding if
;; `package-refresh-contents' needs to be run during startup.  As this
;; can slow things down, it is only run if the archives are considered
;; stale.  Archives are considered stale (by default) when they are 1
;; day old.  Set the `crafted-package-update-days' to a larger value
;; in your `early-init' file to changes this behavior
(require 'time-date)

(defvar crafted-package-perform-stale-archive-check t
  "Check if any package archives are stale.

Set this value in your `early-init.el' file.")

(defvar crafted-package-update-days 1
  "Number of days before an archive will be considered stale.

Set this value in your `early-init.el' file")

(setq-local archives-path (expand-file-name "archives" package-user-dir))

(defun crafted-package-archive-stale-p (archive)
  "Return t if ARCHIVE is stale.

ARCHIVE is stale if the on-disk cache is older than
`crafted-package-update-days' old.  If
`crafted-package-perform-stale-archive-check' is nil, the check
is skipped"
  (let* ((today (decode-time nil nil t))
         (archive-name (format "%s/%s/archive-contents" archives-path archive))
         (last-update-time (decode-time (file-attribute-modification-time
                                         (file-attributes archive-name))))
         (delta (make-decoded-time :day crafted-package-update-days)))
    (message "%s archive-name %s" context archive-name)
    (when crafted-package-perform-stale-archive-check
      (time-less-p (encode-time (decoded-time-add last-update-time delta))
                   (encode-time today)))))

(defun crafted-package-archives-stale-p ()
  "Return t if any package archives' cache is out of date.

Check each archive listed in `package-archives', if the on-disk
cache is older than `crafted-package-update-days', return a
non-nil value.  Fails fast, will return t for the first stale
archive found or nil if they are all up-to-date"
  (interactive)
  (cl-some #'crafted-package-archive-stale-p (mapcar #'car package-archives)))

(defun crafted-package-initialize ()
  "Initialize the package system.

Run this in the `before-init-hook'"

  (when package-enable-at-startup
    (package-initialize)

    (require 'seq)
    (message "%s checking package archives in %s" context archives-path)
    (cond ((seq-empty-p package-archive-contents)
           (progn
             (message "%s package archives empty, initalizing" context)
             (package-refresh-contents)))
          ((crafted-package-archives-stale-p)
           (progn
             (message "%s package archives stale, refreshing" context)
             (package-refresh-contents t)))
          (t
           (message "%s package archives fresh, do nothing" context)))
    (message "%s package system initialized!" context)))

;;; Initialize package system
;; Refresh archives if necessary before init file runs.
(add-hook 'before-init-hook #'crafted-package-initialize)

(provide 'crafted-early-init-config)
;;; crafted-early-init-config.el ends here
