;;;; keypad.el -- define keypad keys for using keypad almost in isolation
;;; Time-stamp: <2021-04-03 16:06:30 jcgs>

;; Copyright (C) 2007, 2021, John C. G. Sturdy

;; Author: John C. G. Sturdy <john@cb1.com>
;; Maintainer: John C. G. Sturdy <john@cb1.com>
;; Created: 2007?
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;;; todo: rename to versor-keypad.el

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;; Commentary:
;; 

;;; Code:
(defun versor-keypad-setup ()
  "Set up the keypad for being the main editing input."
  (interactive)
  (global-set-key [ pause ] 'handsfree-main-menu)
  (global-set-key [ C-pause ] 'versor-do-dynamic-menu)
  ;; fill in the rest
)

(provide 'keypad)

;;; keypad.el ends here
