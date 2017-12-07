;;; joystick-chord-kbd.el --- convert joystick chords to characters

;; Copyright (C) 2008  John Sturdy

;; Author: John Sturdy <john.sturdy@ul.ie>
;; Keywords: hardware, terminals

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This takes chords read by joystick.el, and converts them into
;; character input.  This lets you use a re-arranged gamepad as a
;; Braille or GKOS keyboard.

;;; Code:

(defun joystick-chord-construct-bit-arranger (bits)
  "Return a function that packs BITS into the low bits of a number."
  (let ((bit-offset 0)
	(elements-list nil))
    (dolist (bit bits)
      (push `(if (zerop (logand chord ,(ash 1 bit)))
		 0
	       ,(ash 1 bit-offset))
	    elements-list)
      (setq bit-offset (1+ bit-offset)))
    `(lambda (chord)
       (logior ,@(nreverse elements-list)))))

(defvar joystick-chord-arranged-table nil
  "Table of characters corresponding to chords.
This caches the calculation from re-arranging bits in the chord.")

(defvar joystick-chord-down-shift 0
  "How far to shift the incoming chords down.")

(defvar joystick-chord-mask 255
  "Mask for the incoming chords.
It is applied after they have been shifted `joystick-chord-down-shift' bits.")

(defvar debug-chords nil
  "Whether to output messages describing what the chord system is doing.")

(defun joystick-chord-typing-handler (chord)
  "Try to process CHORD as a typed character."
  (let ((chord-bits (logand (ash chord joystick-chord-down-shift)
			    joystick-chord-mask)))
    (when debug-chords
      (message "Raw chord 0x%x transformed to 0x%x" chord chord-bits))
    (if (zerop chord-bits)
	nil
      (let ((char (aref joystick-chord-arranged-table
			chord-bits)))
	(when debug-chords
	  (message "Chord 0x%x produces character 0x%x" chord-bits char))
	(if (zerop char)
	    nil
	  (setq unread-command-events
		(nreverse
		 (cons char
		       (nreverse
			unread-command-events))))
	  t)))))

(defgroup joystick-chord-keyboard
  nil
  "Options for using a joystick as a chord keyboard.")

(defcustom joystick-chord-kbd-device (if (file-exists-p "/dev/js1")
					 "/dev/js1"
				       "/dev/input/js1")
  "The device to use for chord keyboard input."
  :group 'joystick-chord-keyboard
  :type '(file :must-match t))

;;;###autoload
(defun joystick-chord-kbd-setup (bits table)
  "Initialize the chord keyboard reader.
BITS is a list of the chord bits that are used, as bit offsets.
TABLE is the table of characters indexed by the number made by
combining the specified bits.  The first element of BITS is the
least significant bit in the index, and so on.
A hook function is set up that will be used in `joystick-start'."
  (let* ((down-shift (apply 'min bits))
	 (top-bit (- (apply 'max bits) down-shift))
	 (slots (ash 2 top-bit))	; aka (ash 1 (1+ top-bit))
	 (arranger (joystick-chord-construct-bit-arranger
		    (mapcar (lambda (bit)
			      (- bit down-shift))
			    bits)))
	 (arranged-table (make-string slots 0)))
    (dotimes (i slots)
      (aset arranged-table
	    i
	    (aref table
		  (funcall arranger
			   i))))
    (add-hook 'joystick-chord-handlers 'joystick-chord-typing-handler)
    (setq joystick-chord-down-shift (- down-shift)
	  joystick-chord-mask (1- slots)
	  joystick-chord-arranged-table arranged-table
	  joystick-reading-chords t))
  (add-hook 'joystick-pre-start-hook
	    (lambda ()
	      (setq joystick-start-extra-args
		    `("--prefix" "Brailler-" "--device" ,joystick-chord-kbd-device)))))

;;;###autoload
(defun joystick-braille-kbd-setup ()
  "Set up the braille typing."
  (interactive)
  (when t
    (let ((i 1))
    (dolist (dot joystick-chord-braille-dots)
      (setq i (1+ i)))))
  (joystick-chord-kbd-setup joystick-chord-braille-dots
			    joystick-chord-braille-characters))

(defvar joystick-chord-braille-dots
  '(12					; dot 1
    13					; dot 2
    15					; dot 3
    
    16					; dot 4
    17					; dot 5
    20					; dot 6
    
    14					; dot 7
    21					; dot 8
    
    )
  "Which bits of chords are used for the Braille dots.
You may have to re-arrange this to suit your re-arranged input device.")

(defvar joystick-braille-character-dots
  '("a1" "b12" "c14" "d145" "e15" "f124" "g1245" "h125" "i24" "j245"
    "k13" "l123" "m134" "n1345" "o135" "p1234" "q12345" "r1235" "s234" "t2345"
    "u136" "v1236" "x1346" "y13456" "z1356" "w246"
    " 7" "-25" ",2" ".256" "!235" "?236" "8" "78"

    ;; now the control characters
    "17" "127" "147" "1457" "157" "1247" "12457" "1257" "\t247" "\n2457"
    "137" "1237" "1347" "13457" "1357" "12347" "123457" "12357" "2347" "23457"
    "1367" "12367" "13467" "134567" "13567" "2467"

    ;; now the meta characters
    "á18" "â128" "ã148" "ä1458" "å158" "æ1248" "ç12458" "è1258" "é248" "ê2458"
;;     "k138" "l1238" "m1348" "n13458" "o1358" "p12348" "q123458" "r12358" "s2348" "t23458"
;;     "u1368" "v12368" "x13468" "y134568" "z13568" "w2468"

    ;; now the control-meta characters
    "178" "1278" "1478" "14578" "1578" "12478" "124578" "12578" "2478" "24578"
;;     "k1378" "l12378" "m13478" "n134578" "o13578" "p123478" "q1234578" "r123578" "s23478" "t234578"
;;     "u13678" "v123678" "x134678" "y1345678" "z135678" "w24678"


    )
  "List of characters and the dots that make them in Braille.
Each entry is a string starting with that character, and followed by the dot numbers.")

(defun joystick-make-braille-table ()
  "Fill in the braille table."
  (let ((table (make-string 256 0)))
    (dolist (char-descr joystick-braille-character-dots)
      (let ((char (aref char-descr 0))
	    (len (length char-descr))
	    (i 1)
	    (code 0))
	(while (< i len)
	  ;; (message "%c has %c" char (aref char-descr i))
	  (setq code (logior code
			     (ash 1
				  (- (aref char-descr i)
				     ?1)))
		i (1+ i)))
	(aset table code char)))
    table))

(defvar joystick-chord-braille-characters
  (joystick-make-braille-table)
  "The table of braille characters.")

(provide 'joystick-chord-kbd)

;;; joystick-chord-kbd.el ends here
