;;; notdeft-global.el --- Global NotDeft keymap  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; A keymap of NotDeft commands usable from outside `notdeft-mode'. It is
;; bound both as a variable and a function, to the name
;; `notdeft-global-map'.

;;; Code:

(defvar notdeft-global-map
  (let ((map (make-sparse-keymap)))
    ;; file management
    (define-key map (kbd "C-n") #'notdeft-new-file)
    (define-key map (kbd "C-m") #'notdeft-new-file-named)
    (define-key map (kbd "C-x C-f") #'notdeft-find-file)
    (define-key map (kbd "C-x C-w") #'notdeft-save-buffer)
    (define-key map (kbd "C-d") #'notdeft-delete-file)
    (define-key map (kbd "C-r") #'notdeft-rename-file)
    (define-key map (kbd "C-v") #'notdeft-move-file)
    (define-key map (kbd "C-x s") #'notdeft-move-into-subdir)
    (define-key map (kbd "C-x e") #'notdeft-change-file-extension)
    (define-key map (kbd "C-a") #'notdeft-archive-file)
    (define-key map (kbd "C-i") #'notdeft-show-file-directory)
    (define-key map (kbd "C-x d") #'notdeft-open-in-deft)
    ;; state
    (define-key map (kbd "C-j") #'notdeft-chdir)
    (define-key map (kbd "C-x g") #'notdeft-refresh)
    (define-key map (kbd "C-x c") #'notdeft-gc)
    (define-key map (kbd "C-x r") #'notdeft-reindex)
    ;; search
    (define-key map (kbd "C-o") #'notdeft-open-query)
    (define-key map (kbd "C-f") #'notdeft-query-select-find-file)
    (define-key map (kbd "C-x o") #'notdeft-lucky-find-file)
    ;; movement
    (define-key map (kbd "C-x b") #'notdeft-switch-to-note-buffer)
    (define-key map (kbd "C-x B") #'notdeft-switch-to-buffer)
    ;; other
    (define-key map (kbd "C-c") #'notdeft)
    map)
  "Global keymap for NotDeft.

\\{notdeft-global-map}")
(fset 'notdeft-global-map notdeft-global-map)

(provide 'notdeft-global)

;;; notdeft-global.el ends here
