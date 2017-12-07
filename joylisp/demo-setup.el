;; Setup of demo of versor with gamepad
;; Time-stamp: <2010-09-22 23:18:34 jcgs>
;; J C G Sturdy 2010-09-22
(setq menu-bar-lines 0)
(tool-bar-mode -1)
(set-frame-font "6x10")
(copy-file "~/common/open-projects/emacs-versor/lisp/versor-dimensions.el" "/tmp/dimensions.el" t)
(copy-file "~/common/open-projects/emacs-versor/lisp/versor-base-moves.el" "/tmp/base-moves.el" t)
(find-file "/tmp/base-moves.el")
(find-file "/tmp/dimensions.el")
(add-to-list 'load-path "~/common/open-projects/emacs-versor/lisp/")
(add-to-list 'load-path "~/common/open-projects/emacs-versor/joylisp/")
(require 'versor)
(require 'joystick)

(versor-setup 'arrows
	      'arrows-misc
	      'keypad
	      'keypad-misc
	      'meta
	      'modal
	      'joystick
	      'menu
	      'quiet-underlying
	      'text-in-code
	      'mouse)


;; demo-setup.el ends here
