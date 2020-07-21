;;; notdeft-install.el --- NotDeft installer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by the authors.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Functionality for setting up NotDeft from its source distribution,
;; without using a package manager.
;;
;; Suggested use:
;;  (require 'notdeft-install)
;;  (notdeft-install)

;;; Code:

(require 'autoload)
(require 'bytecomp)

(declare-function notdeft-xapian-make-program "notdeft-xapian-make")

(defun notdeft-install-autoloads ()
  "Generate NotDeft autoloads and load them."
  (let ((home (file-name-directory
	       (locate-library "notdeft-install"))))
    (let ((generated-autoload-file
	   (expand-file-name "notdeft-autoloads.el" home)))
      (update-directory-autoloads home))
    (load "notdeft-autoloads.el" nil nil t)))

(defun notdeft-install-bytecode (&optional force)
  "Generate NotDeft Emacs Lisp \".elc\" files.
Optionally FORCE byte-compilation even when existing bytecode
files appear to be up-to-date."
  (let ((dir (file-name-directory
	      (locate-library "notdeft-install"))))
    (notdeft-install--byte-compile dir "./" t force)
    (notdeft-install--byte-compile dir "extras/" nil force)))

(defun notdeft-install--byte-compile (dir subdir must force)
  "Byte-compile NotDeft sources in DIR SUBDIR.
If so indicated, the directory MUST exist. Optionally FORCE the
compilation."
  (let ((home (expand-file-name subdir dir)))
    (when (or must (file-exists-p home))
      (let ((files (directory-files home nil "^notdeft.*\\.el$")))
	(dolist (file files)
	  (unless (member file '("notdeft-autoloads.el"
				 "notdeft-example.el"))
	    (let ((file (concat home file)))
	      (byte-recompile-file file force 0))))))))

;;;###autoload
(defun notdeft-install (&optional force)
  "Generate NotDeft autoloads and binaries.
Optionally FORCE byte-compilation even when existing bytecode
files appear to be up-to-date."
  (interactive "P")
  (notdeft-install-autoloads)
  (require 'notdeft-autoloads)
  (notdeft-install-bytecode force)
  (require 'notdeft-xapian-make)
  (notdeft-xapian-make-program force))

(provide 'notdeft-install)

;;; notdeft-install.el ends here
