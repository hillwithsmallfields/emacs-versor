;;;; pedals.el -- set up the six-pedal system
;;; Time-stamp: <2018-03-01 16:23:32 jcgs>
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2017, 2018  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;;
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'versor)
(require 'versor-menu)

(defvar pedals-hosts-preferring-num-lock
  nil
  "Hosts on which the pedals appear to work better with the keypad in num lock mode.")

(defun handsfree-use-num-lock ()
  "Whether the pedals appear to work better with the keypad in num lock mode on this system."
  (member (downcase (system-name)) pedals-hosts-preferring-num-lock))

(defvar pedal-aux nil
  "The aux pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-aux nil
  "The C-aux pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-M-aux nil
  "The M-aux pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-S-aux nil
  "The S-aux pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-S-aux nil
  "The C-S-aux pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-M-S-aux nil
  "The M-S-aux pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-M-S-aux nil
  "The C-M-S-aux pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-onward nil
  "The onward pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-onward nil
  "The C-onward pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-M-onward nil
  "The M-onward pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-S-onward nil
  "The S-onward pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-S-onward nil
  "The C-S-onward pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-M-S-onward nil
  "The M-S-onward pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-M-S-onward nil
  "The C-M-S-onward pedal.
This symbol may be given inside a vector to define-key etc")


(defvar pedal-menu nil
  "The menu pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-menu nil
  "The C-menu pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-M-menu nil
  "The M-menu pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-S-menu nil
  "The S-menu pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-S-menu nil
  "The C-S-menu pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-M-S-menu nil
  "The M-S-menu pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedal-C-M-S-menu nil
  "The C-M-S-menu pedal.
This symbol may be given inside a vector to define-key etc")

(defvar pedals-use-kp-divide nil
  ;; maybe should be t on WinNT and nil elsewhere?
  "Whether to use kp-divide instead of kp-down")

(defvar pedal-code-names
  '(pedal-onward
    pedal-C-onward
    pedal-M-onward
    pedal-S-onward
    pedal-C-S-onward
    pedal-M-S-onward
    pedal-C-M-S-onward
    pedal-aux
    pedal-C-aux
    pedal-M-aux
    pedal-S-aux
    pedal-C-S-aux
    pedal-M-S-aux
    pedal-C-M-S-aux
    pedal-menu
    pedal-C-menu
    pedal-M-menu
    pedal-S-menu
    pedal-C-S-menu
    pedal-M-S-menu
    pedal-C-M-S-menu)
  "The variable names we use to hold the pedal codes.")

(defvar pedals-code-alist
  nil
  "Alist mapping pedal variable names to the keys used for those pedals.")

(defun pedals-make-code-alist ()
  "Generate the value for pedals-code-alist"
  (mapcar (function
	   (lambda (name)
	     (let ((value (symbol-value name)))
	       (if (arrayp value)
		   (cons name (aref value 0))
		 (message "No key seems to be assigned to %S" name)
		 (cons name nil)))))
	  pedal-code-names))

(defun pedal-name (keyname)
  "Return the pedal name for KEYNAME."
  (or (car (rassoc keyname pedals-code-alist))
      keyname))

;; todo: write around-advice on key-description, to give the pedal names by calling pedal-name where appropriate

(defun pedals-use-kp-divide ()
  "Set the pedals up for a machine using kp-divide for onward."
  (interactive)
  (message "Setting up pedals using kp-divide for onward")
  (setq pedal-onward [ kp-divide ]
	pedal-C-onward [ C-kp-divide ]
	pedal-M-onward [ M-kp-divide ]
	pedal-S-onward [ S-kp-divide ]
	pedal-C-S-onward [ C-S-kp-divide ]
	pedal-M-S-onward [ M-S-kp-divide ]
	pedal-C-M-S-onward [ C-M-S-kp-divide ]

	pedal-aux [ kp-end ]
	pedal-C-aux [ C-kp-end ]
	pedal-M-aux [ M-kp-end ]
	pedal-S-aux [ S-kp-end ]	; which machine wants this?
	pedal-S-aux [ S-kp-1 ]
	pedal-C-S-aux [ C-S-kp-end ]
	pedal-M-S-aux [ M-S-kp-end ] ; unfortunately not distinguishable by help?
	pedal-C-M-S-aux [ C-M-S-kp-1 ]

	pedal-menu [ kp-next ]
	pedal-C-menu [ C-kp-next ]
	pedal-M-menu [ M-kp-next ]
	pedal-S-menu [ S-kp-next ]	; which machine wants this?
	pedal-S-menu [ S-kp-3 ]
	pedal-C-S-menu [ C-S-kp-next ]
	pedal-M-S-menu [ M-S-kp-next ]
	pedal-C-M-S-menu [ C-M-S-kp-next ]))

(defun pedals-use-linux-layout ()
  "Set the pedals up as for a typical gnu/linux machine."
  (interactive)
  (message "Setting up pedals as for gnu/linux.")
  (setq pedal-aux [ kp-end ]
	pedal-S-aux [ S-kp-1 ]
	pedal-C-aux [ C-kp-end ]
	pedal-C-S-aux [ C-S-kp-1 ]
	pedal-M-aux [ M-kp-end ]
	pedal-M-S-aux [ M-S-kp-1 ]
	pedal-C-M-aux [ C-M-kp-end ]
	pedal-C-M-S-aux [ C-M-S-kp-1 ]

	pedal-onward [ kp-down ]
	pedal-S-onward [ S-kp-2 ]
	pedal-C-onward [ C-kp-down ]
	pedal-C-S-onward [ C-S-kp-2 ]
	pedal-M-onward [ M-kp-down ]
	pedal-M-S-onward [ M-kp-kp-2 ]
	pedal-C-M-onward [ C-M-kp-down ]
	pedal-C-M-S-onward [ C-M-S-kp-2 ]

	pedal-menu [ kp-next ]
	pedal-S-menu [ S-kp-3 ]
	pedal-C-menu [ C-kp-next ]
	pedal-C-S-menu [ C-S-kp-3 ]
	pedal-M-menu [ M-kp-next ]
	pedal-M-S-menu [ M-S-kp-3 ]
	pedal-C-M-menu [ C-M-kp-next ]
	pedal-C-M-S-menu [ C-M-S-kp-3 ]))

(defun pedals-use-default-layout ()
  "Set pedals up using old default pedal setup."
  (interactive)
  (message "Using old default pedal setup")
  (setq pedal-onward [ kp-down ]
	pedal-C-onward [ C-kp-down ]
	pedal-M-onward [ M-kp-down ]
	pedal-S-onward [ S-kp-2 ]
	pedal-C-S-onward [ C-S-kp-2 ]
	pedal-M-S-onward [ M-S-kp-2 ]
	pedal-C-M-S-onward [ C-M-S-kp-2 ]

	pedal-aux [ kp-end ]
	pedal-C-aux [ C-kp-end ]
	pedal-M-aux [ M-kp-end ]
	pedal-S-aux [ S-kp-1 ]
	pedal-C-S-aux [ C-S-kp-1 ]
	pedal-M-S-aux [ M-S-kp-1 ]
	pedal-C-M-S-aux [ C-M-S-kp-1 ]

	pedal-menu [ kp-next ]
	pedal-C-menu [ C-kp-next ]
	pedal-M-menu [ M-kp-next ]
	pedal-S-menu [ S-kp-3 ]
	pedal-C-S-menu [ C-S-kp-3 ]
	pedal-M-S-menu [ M-S-kp-3 ]
	pedal-C-M-S-menu [ C-M-S-kp-3 ]))

(defun pedals-current ()
  "An interactively-created pedals setup."
  (interactive)
  (setq pedal-onward [kp-2]
        pedal-C-onward [C-kp-2]
        pedal-M-onward [M-kp-2]
        pedal-S-onward [S-kp-down]
        pedal-C-S-onward [C-S-kp-down]
        pedal-M-S-onward [M-S-kp-down]
        pedal-aux [kp-1]
        pedal-C-aux [C-kp-1]
        pedal-M-aux [M-kp-1]
        pedal-S-aux [S-kp-end]
        pedal-C-S-aux [C-S-kp-end]
        pedal-menu [kp-3]
        pedal-C-menu [C-kp-3]
        pedal-M-menu [M-kp-3]
        pedal-S-menu [S-kp-next]
        pedal-C-S-menu [C-kp-3]
        pedal-M-S-menu [M-S-kp-next]))

(defun define-key-if-known (map key definition)
  "In MAP, if KEY is known, bind it to DEFINITION."
  (when key
    (define-key map key definition)))

(defun global-set-key-if-known (key definition)
  "If KEY is known, bind it globally to DEFINITION."
  (when key
    (global-set-key key definition)))

(defun pedals-setup-codes ()
  "Set up the pedal codes."
  ;; I had great trouble getting a setup that would work on NTemacs,
  ;; but eventually found that it was describe-key that was giving the
  ;; impression that the Shift modifier did not work on the keypad
  ;; arrow keys. Hence, all this stuff about num-lock, which I hope to
  ;; get rid of once I've verified that it is in fact OK.
  (interactive)
  (let ((num-lock (handsfree-use-num-lock)))
    (cond
     ((fboundp 'pedals-current) (pedals-current))
     (pedals-use-kp-divide
      (pedals-use-kp-divide))
     ((or (eq system-type 'gnu/linux)
	  (equal (downcase (system-name)) "mayo"))
      (pedals-use-linux-layout))
     (t
      (pedals-use-default-layout))))
  (setq pedals-code-alist (pedals-make-code-alist)))

(defun pedals-test-keys ()
  "Read key events and display what they were.
Useful for exploring which keys can be read with all modifiers,
which are most suitable for duplicating onto pedals."
  (interactive)
  (let ((key (read-event "Key (space to end): ")))
    (while (not (equal key ? ))
      (setq key (read-event (format "That was %s; next key (space to end): "
				    (key-description (list key))))))))

(defun dired-define-pedals ()
  "Set up pedal definitions for dired."
  (eval-after-load "dired"
    '(progn
      (define-key-if-known dired-mode-map pedal-onward 'dired-next-line)
      (define-key-if-known dired-mode-map pedal-S-onward 'dired-previous-line)
      (define-key-if-known dired-mode-map pedal-C-onward 'dired-prev-subdir)
      (define-key-if-known dired-mode-map pedal-C-S-onward 'dired-next-subdir)
      (define-key-if-known dired-mode-map pedal-M-onward 'dired-tree-up)
      (define-key-if-known dired-mode-map pedal-M-S-onward 'dired-tree-down)
      (define-key-if-known dired-mode-map pedal-menu 'dired-find-file-or-insert-subdir)
      (define-key-if-known dired-mode-map pedal-S-menu 'dired-flag-file-deletion)
      (define-key-if-known dired-mode-map [ C-kp-3 ] 'revert-buffer)
      (define-key-if-known dired-mode-map pedal-C-S-menu 'dired-do-flagged-delete)
      (define-key-if-known dired-mode-map pedal-M-S-menu 'kill-buffer))))

(defun vm-define-pedals ()
  "Set up pedal definitions for vm."
  (eval-after-load "vm"
    '(progn
      (define-key-if-known vm-summary-mode-map pedal-aux 'vm-next-message)
      (define-key-if-known vm-summary-mode-map pedal-S-aux 'vm-previous-message)
      (define-key-if-known vm-summary-mode-map pedal-onward 'vm-scroll-forward)
      (define-key-if-known vm-summary-mode-map pedal-S-onward 'vm-scroll-backward)
      (define-key-if-known vm-summary-mode-map pedal-menu 'exit-recursive-edit ) ; because I normally run the mailer in a recursive edit
      (define-key-if-known vm-summary-mode-map pedal-S-menu 'vm-delete-message)
      (define-key-if-known vm-summary-mode-map pedal-C-S-menu 'vm-expunge-folder)
      (define-key-if-known vm-summary-mode-map pedal-M-menu 'handsfree-main-menu)
      (define-key-if-known vm-summary-mode-map pedal-M-S-menu 'vm-delete-as-spam)
      (setq vm-pedals-configured t)
      )))

(defun w3-define-pedals ()
  "Set up pedal definitions for w3."
  (eval-after-load "w3"
    '(progn
      (define-key-if-known w3-mode-map pedal-onward 'scroll-up)
      (define-key-if-known w3-mode-map pedal-S-onward 'scroll-down)
      (define-key-if-known w3-mode-map pedal-aux 'w3-widget-forward)
      (define-key-if-known w3-mode-map pedal-S-aux 'w3-widget-backward)
      (define-key-if-known w3-mode-map pedal-menu 'widget-button-press)
      (define-key-if-known w3-mode-map pedal-S-menu 'w3-prev-document))))

(defun gnus-define-pedals ()
  "Set up pedal definitions for gnus."
  (eval-after-load "gnus"
    '(progn
      (define-key-if-known gnus-group-mode-map pedal-aux 'gnus-group-catchup-current)
      (define-key-if-known gnus-group-mode-map pedal-S-aux 'gnus-group-exit)
      (define-key-if-known gnus-group-mode-map pedal-onward 'next-line)
      (define-key-if-known gnus-group-mode-map pedal-S-onward 'previous-line)
      (define-key-if-known gnus-group-mode-map pedal-menu 'gnus-group-select-group)
      (define-key-if-known gnus-group-mode-map pedal-S-menu 'gnus-group-read-group)
      (if (and (boundp 'gnus-summary-mode-map)
	       (keymapp gnus-summary-mode-map))
	  (progn
	    (define-key-if-known gnus-summary-mode-map pedal-aux 'gnus-summary-tick-article-forward)
	    (define-key-if-known gnus-summary-mode-map pedal-S-aux 'gnus-summary-followup)
	    (define-key-if-known gnus-summary-mode-map pedal-onward 'gnus-summary-next-page)
	    (define-key-if-known gnus-summary-mode-map pedal-S-onward 'gnus-summary-previous-page)
	    (define-key-if-known gnus-summary-mode-map pedal-menu 'gnus-summary-exit)
	    (define-key-if-known gnus-summary-mode-map pedal-S-menu 'gnus-summary-catchup-and-exit))))))

(defun yank-menu-define-pedals ()
  "Set up pedals for yank-menu."
  (eval-after-load "yank-menu"
    '(progn
      (define-key-if-known yank-menu-map pedal-onward 'yank-menu-next)
      (define-key-if-known yank-menu-map pedal-S-onward 'yank-menu-previous)
      (define-key-if-known yank-menu-map pedal-M-S-onward 'yank-menu-top)
      (define-key-if-known yank-menu-map pedal-M-onward 'yank-menu-bottom)
      (define-key-if-known yank-menu-map pedal-menu 'yank-menu-insert)
      (define-key-if-known yank-menu-map pedal-S-menu 'yank-menu-quit))))

(defun autocue-define-pedals ()
  "Set up pedals for autocue."
  (eval-after-load "autocue"
    '(progn
      (define-key-if-known autocue-keymap pedal-C-aux 'autocue:faster)
      (define-key-if-known autocue-keymap pedal-C-S-aux 'autocue:slower)
      (define-key-if-known autocue-keymap pedal-aux 'autocue:bigger)
      (define-key-if-known autocue-keymap pedal-S-aux 'autocue:smaller)
      (define-key-if-known autocue-keymap pedal-onward 'scroll-up)
      (define-key-if-known autocue-keymap pedal-S-onward 'scroll-down)
      (define-key-if-known autocue-keymap pedal-menu 'autocue:put-aside))))

(defun view-mode-define-pedals ()
  "Set up pedals for view-mode."
  (eval-after-load "view"
    '(unless (and (boundp 'view-mode-defined-pedals)
		  view-mode-defined-pedals)
       (setq view-mode-defined-pedals t)
       (define-key-if-known view-mode-map pedal-onward 'View-scroll-page-forward)
       (define-key-if-known view-mode-map pedal-S-onward 'View-scroll-page-backward)
       (define-key-if-known view-mode-map pedal-menu 'View-quit))))

(defun info-define-pedals ()
  "Helper function for pedals-setup."
  (eval-after-load "info"
    '(progn
      (define-key-if-known Info-mode-map pedal-aux 'Info-forward-node)
      (define-key-if-known Info-mode-map pedal-S-aux 'Info-backward-node)
      (define-key-if-known Info-mode-map pedal-C-aux 'Info-follow-reference)
      (define-key-if-known Info-mode-map pedal-C-S-aux 'Info-last)
      (define-key-if-known Info-mode-map pedal-onward 'Info-scroll-up)
      (define-key-if-known Info-mode-map pedal-S-onward 'Info-scroll-down)
      (define-key-if-known Info-mode-map pedal-menu 'Info-exit)
      (define-key-if-known Info-mode-map pedal-S-menu 'Info-menu))))

(defvar pedal:versor-change-dimension-ctrl nil
  "*Whether the control pedal should make versor movements switch dimension.
This is the function normally assigned to meta and a versor movement, but
pedal physical layout may make it more convenient to put it on control.")

(defun comint-define-pedals ()
  "Helper function for pedals-setup."
  (eval-after-load "comint"
    '(progn
      (define-key-if-known comint-mode-map pedal-onward 'handsfree-comint-onwards)
      (define-key-if-known comint-mode-map pedal-S-onward 'handsfree-comint-backwards)
      (define-key-if-known comint-mode-map pedal-S-menu 'handsfree-comint-choose-or-return))))

(defun pedals-setup ()
  "Set up the six-switch (or nine-switch) system.
In general:
  the middle pedal of the right cluster does fine movements;
  the right pedal brings up a menu or picks an item;
  the left pedal does coarse movements.
See versor.el (versatile cursors) for fine and coarse movements.
See handsfree-menus.el for menus."
  (interactive)

  (versor-setup)

  (pedals-setup-codes)

  ;; left pedal of right cluster -- misc things at point
  (global-set-key-if-known pedal-aux 'versor-over-next)
  (global-set-key-if-known pedal-S-aux 'versor-over-prev)

  (global-set-key-if-known pedal-C-aux (if pedal:versor-change-dimension-ctrl
				  'versor-next-meta-level
				'versor-dwim ; 'versor-other-end-of-item
				))
  (global-set-key-if-known pedal-C-S-aux (if pedal:versor-change-dimension-ctrl
				    'versor-prev-meta-level
				  'wander-yank-dwim))

  (global-set-key-if-known pedal-M-aux (if pedal:versor-change-dimension-ctrl
				  'versor-dwim ; 'versor-other-end-of-item
				'versor-next-meta-level))
  (global-set-key-if-known pedal-M-S-aux (if pedal:versor-change-dimension-ctrl
				    'wander-yank-dwim
				  'versor-prev-meta-level))

  ;; middle pedal of right cluster -- dimensional navigation
  (global-set-key-if-known pedal-onward 'versor-next)
  (global-set-key-if-known pedal-S-onward 'versor-prev)

  (global-set-key-if-known pedal-C-onward (if pedal:versor-change-dimension-ctrl
				     'versor-in
				   'versor-extend-item-forwards))
  (global-set-key-if-known pedal-C-S-onward (if pedal:versor-change-dimension-ctrl
				       'versor-out
				     'versor-extend-item-backwards))

  (global-set-key-if-known pedal-M-onward (if pedal:versor-change-dimension-ctrl
				     'versor-extend-item-forwards
				   'versor-in))
  (global-set-key-if-known pedal-M-S-onward (if pedal:versor-change-dimension-ctrl
				       'versor-extend-item-backwards
				     'versor-out))

  ;; right pedal of right cluster -- menus, yank, repeat, undo, quit
  (global-set-key-if-known pedal-menu 'handsfree-main-menu)
  ;;  (global-set-key-if-known pedal-S-menu 'handsfree-popup-tools-menu)
  (global-set-key-if-known pedal-S-menu 'versor-do-dynamic-menu)

  (global-set-key-if-known pedal-C-menu 'other-window-or-buffer)
  (global-set-key-if-known pedal-C-S-menu 'repeat-complex-command )

  (global-set-key-if-known pedal-M-menu 'versor-begin-altering-item)
  (global-set-key-if-known pedal-M-S-menu 'keyboard-quit )

  ;;  (global-set-key-if-known pedal-C-M-S-aux 'describe-key )
  ;;  (global-set-key-if-known pedal-C-M-S-onward 'foo )

  ;; minibuffer -- left pedal is "expand", middle is "navigate", right is "do it"
  (mapcar (lambda (map-name)
	    (let* ((map (eval map-name))
		   (upkey (lookup-key map [ up ]))
		   (downkey (lookup-key map [ down ]))
		   (returnkey (lookup-key map "\n")))
	      (message "in map %S" map-name)
	      (message "  down is %S, up is %S, return is %S" downkey upkey returnkey)
	      (define-key-if-known map pedal-aux
		'minibuffer-blank ;; (lookup-key map " ")
		)
	      (define-key-if-known map pedal-S-aux (lookup-key map "\t"))

	      (message "  Defining %S to do %S" pedal-onward downkey)
	      (define-key-if-known map pedal-onward downkey)
	      (message "  Defining %S to do %S" pedal-S-onward upkey)
	      (define-key-if-known map pedal-S-onward upkey)
	      (define-key-if-known map pedal-C-S-onward 'keyboard-escape-quit)

	      (define-key-if-known map pedal-menu returnkey)
	      (define-key-if-known map pedal-S-menu returnkey)))
	  '(minibuffer-local-map
	    minibuffer-local-ns-map
	    minibuffer-local-completion-map
	    minibuffer-local-must-match-map
	    read-expression-map))

  ;; modal -- generally right pedal is "do it", shift-right is "delete"
  ;;                    middle pedal is "next"
  ;;                    left pedal is "do something else with it"
  (define-key-if-known isearch-mode-map pedal-onward 'isearch-repeat-forward)
  (define-key-if-known isearch-mode-map pedal-aux 'isearch-yank-word)
  (define-key-if-known isearch-mode-map pedal-S-onward 'isearch-repeat-backward)
  (define-key-if-known isearch-mode-map pedal-menu 'isearch-exit)
  (define-key-if-known isearch-mode-map pedal-S-menu 'isearch-cancel)

  (if (and (boundp 'versor-altering-mode-map)
	   (keymapp versor-altering-mode-map))
      (progn
	(define-key-if-known versor-altering-mode-map pedal-onward 'versor-alter-item-next-value)
	(define-key-if-known versor-altering-mode-map pedal-S-onward 'versor-alter-item-previous-value)
	(define-key-if-known versor-altering-mode-map pedal-aux 'versor-alter-item-next-type)
	(define-key-if-known versor-altering-mode-map pedal-S-aux 'versor-alter-item-previous-type)
	(define-key-if-known versor-altering-mode-map pedal-menu 'versor-end-altering-item)
	(define-key-if-known versor-altering-mode-map pedal-S-menu 'versor-abandon-altering-item)))

  (if (and (boundp 'Buffer-menu-mode-map)
	   (keymapp Buffer-menu-mode-map))
      (progn
	(define-key-if-known Buffer-menu-mode-map pedal-onward 'next-line)
	(define-key-if-known Buffer-menu-mode-map pedal-S-onward 'previous-line)
	(define-key-if-known Buffer-menu-mode-map pedal-M-onward 'end-of-buffer)
	(define-key-if-known Buffer-menu-mode-map pedal-M-S-onward 'beginning-of-buffer)
	(define-key-if-known Buffer-menu-mode-map pedal-menu 'Buffer-menu-select)
	(define-key-if-known Buffer-menu-mode-map pedal-S-menu 'Buffer-menu-delete)))

  (dired-define-pedals)
  (vm-define-pedals)
  (w3-define-pedals)
  (gnus-define-pedals)
  (info-define-pedals)
  (yank-menu-define-pedals)
  (comint-define-pedals)
  (autocue-define-pedals)
  (view-mode-define-pedals))

(defun pedals-draw-bindings (&optional keymap)
  "Draw the pedal bindings, in the main keymap or (from a program) the one given.
Optional argument KEYMAP is the keymap to use."
  (interactive)
  (if (null keymap) (setq keymap global-map))
  (let* ((aux (lookup-key keymap pedal-aux))
	 (S-aux (lookup-key keymap pedal-S-aux))
	 (C-aux (lookup-key keymap pedal-C-aux))
	 (C-S-aux (lookup-key keymap pedal-C-S-aux))
	 (M-aux (lookup-key keymap pedal-M-aux))
	 (M-S-aux (lookup-key keymap pedal-M-S-aux))
	 (C-M-aux (lookup-key keymap pedal-C-M-aux))
	 (C-M-S-aux (lookup-key keymap pedal-C-M-S-aux))
	 (aux-symbols (list aux
			    S-aux
			    C-aux
			    C-S-aux
			    M-aux
			    M-S-aux
			    C-M-aux
			    C-M-S-aux))
	 (aux-longest (apply 'max
			     (mapcar 'length
				     (mapcar 'symbol-name
					     aux-symbols))))

	 (onward (lookup-key keymap pedal-onward))
	 (S-onward (lookup-key keymap pedal-S-onward))
	 (C-onward (lookup-key keymap pedal-C-onward))
	 (C-S-onward (lookup-key keymap pedal-C-S-onward))
	 (M-onward (lookup-key keymap pedal-M-onward))
	 (M-S-onward (lookup-key keymap pedal-M-S-onward))
	 (C-M-onward (lookup-key keymap pedal-C-S-onward))
	 (C-M-S-onward (lookup-key keymap pedal-C-M-S-onward))
	 (onward-symbols (list onward
			       S-onward
			       C-onward
			       C-S-onward
			       M-onward
			       M-S-onward
			       C-M-onward
			       C-M-S-onward))
	 (onward-longest (apply 'max
				(mapcar 'length
					(mapcar 'symbol-name
						onward-symbols))))
	 (menu (lookup-key keymap pedal-menu))
	 (S-menu (lookup-key keymap pedal-S-menu))
	 (C-menu (lookup-key keymap pedal-C-menu))
	 (C-S-menu (lookup-key keymap pedal-C-S-menu))
	 (M-menu (lookup-key keymap pedal-M-menu))
	 (M-S-menu (lookup-key keymap pedal-M-S-menu))
	 (C-M-menu (lookup-key keymap pedal-C-M-menu))
	 (C-M-S-menu (lookup-key keymap pedal-C-M-S-menu))
	 (menu-symbols		      (list menu
					    S-menu
					    C-menu
					    C-S-menu
					    M-menu
					    M-S-menu
					    C-M-menu
					    C-M-S-menu))
	 (menu-longest (apply 'max
			      (mapcar 'length
				      (mapcar 'symbol-name
					      menu-symbols))))
	 (descr-format (format "%%6s | | %%s | %%s | %%%ds | | | | %%%ds | %%%ds | %%s | |\n"
			       menu-longest aux-longest onward-longest))
	 (edge-format (format "       | +---+   +-%s-+ | | +-%s-+ %s +---+ |\n"
			      (make-string menu-longest ?-)
			      (make-string aux-longest ?-)
			      (make-string onward-longest ? )))
	 (outer-format  (format "       |           %s   | |  %s     %s      |\n"
				(make-string menu-longest ? )
				(make-string aux-longest ? )
				(make-string onward-longest ? )))
	 (outer-edge-format  (format "       +-------%s-------+ +---%s-----%s-----+\n"
				     (make-string menu-longest ?-)
				     (make-string aux-longest ?-)
				     (make-string onward-longest ?-)))

	 )
    (message "aux-symbols are %S with lengths %S and longest is %d" aux-symbols (mapcar 'length (mapcar 'symbol-name aux-symbols)) aux-longest)
    (message "onward-symbols are %S with lengths %S and longest is %d" onward-symbols (mapcar 'length (mapcar 'symbol-name onward-symbols)) onward-longest)
    (message "menu-symbols are %S with lengths %S and longest is %d" menu-symbols (mapcar 'length (mapcar 'symbol-name menu-symbols)) menu-longest)
    (message "descr-format is %S" descr-format)
    (with-output-to-temp-buffer "*Pedal bindings*"
      (princ outer-edge-format)
      (princ outer-format)
      (princ edge-format)
      (princ (format descr-format
		     "C-M-S"
		     "C" " "
		     (symbol-name C-M-S-menu)
		     (symbol-name C-M-S-aux)
		     (symbol-name C-M-S-onward)
		     " "))
      (princ (format descr-format
		     "C-M"
		     "O" "S"
		     (symbol-name C-M-menu)
		     (symbol-name C-M-aux)
		     (symbol-name C-M-onward)
		     " "))
      (princ (format descr-format
		     "M-S"
		     "N" "H"
		     (symbol-name M-S-menu)
		     (symbol-name M-S-aux)
		     (symbol-name M-S-onward)
		     "M"))
      (princ (format descr-format
		     "C-S"
		     "T" "I"
		     (symbol-name C-S-menu)
		     (symbol-name C-S-aux)
		     (symbol-name C-S-onward)
		     "E"))
      (princ (format descr-format
		     "M"
		     "R" "F"
		     (symbol-name M-menu)
		     (symbol-name M-aux)
		     (symbol-name M-onward)
		     "T"))
      (princ (format descr-format
		     "C"
		     "O" "T"
		     (symbol-name C-menu)
		     (symbol-name C-aux)
		     (symbol-name C-onward)
		     "A"))
      (princ (format descr-format
		     "S"
		     "L" " "
		     (symbol-name S-menu)
		     (symbol-name S-aux)
		     (symbol-name S-onward)
		     " "))
      (princ (format descr-format
		     ""
		     " " " "
		     (symbol-name menu)
		     (symbol-name aux)
		     (symbol-name onward)
		     " "))
      (princ edge-format)
      (princ outer-format)
      (princ outer-edge-format)
      )))

(require 'pp)

(defun pedals-read-interactively ()
  "Find out which pedal is which by getting the user to press them."
  (interactive)
  (let ((setup-items nil))
    (catch 'done
      (dolist (pedal pedal-code-names)
        (let ((pressed (read-event (format "Press %S" pedal))))
          (message "%S ---> %S" pedal pressed)
          (case pressed
            (32 nil)
            (46 (throw 'done t))
            (t (set pedal pressed)
               (push (list pedal (vector pressed))
                     setup-items))))))
    (let ((setup `(defun pedals-current ()
                    "An interactively-created pedals setup."
                    (interactive)
                    ,(cons 'setq
                           (apply 'concatenate 'list
                                  (reverse setup-items))))))
      (with-output-to-temp-buffer "*Pedals setup code*"
        (pp setup))
      setup)))

(provide 'pedals)

;;; end of pedals.el
