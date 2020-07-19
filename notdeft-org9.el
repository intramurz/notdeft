;;; notdeft-org9.el --- Org link support for NotDeft notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Support for "deft:" and "notdeft:" links for `org-mode' version 9.
;; The `org-link-set-parameters' API is available since Org version 9,
;; in the `org' feature.

;;; Code:

(require 'notdeft-org)

(org-link-set-parameters
 "deft"
 :follow #'notdeft-org-open-deft-link
 :complete #'notdeft-org-complete-deft-link)

(org-link-set-parameters
 "notdeft"
 :follow #'notdeft-org-open-notdeft-link
 :store #'notdeft-org-store-notdeft-link)

(provide 'notdeft-org9)

;;; notdeft-org9.el ends here
