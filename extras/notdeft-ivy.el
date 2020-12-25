;;; notdeft-ivy.el --- Ivy completion for NotDeft  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; An Ivy-based implementation of `notdeft-completing-read-function'.
;;
;; Suggested use:
;;  (require 'notdeft-ivy)
;;  (add-to-list 'ivy-re-builders-alist '(notdeft-ivy-completing-read . ivy--regex-ignore-order))
;;  (setq notdeft-completing-read-function 'notdeft-ivy-completing-read)

;;; Code:

(require 'ivy)

(defun notdeft-ivy-completing-read (files &optional prompt)
  "Present a choice of FILES with `ivy-read'.
Only present the non-directory component of each file. There may
be duplicates of the same non-directory name. If non-nil, use the
specified PROMPT. Return the path of the selected file."
  (let* ((choices
	  (mapcar
	   (lambda (file)
	     (propertize (file-name-nondirectory file) 'path file))
	   files))
	 (file
	  (get-text-property
	   0 'path
	   (ivy-read
	    (or prompt "File: ")
	    choices
	    :history 'notdeft-find-file-history
	    :require-match t
	    :caller 'notdeft-ivy-completing-read))))
    file))

(provide 'notdeft-ivy)

;;; notdeft-ivy.el ends here
