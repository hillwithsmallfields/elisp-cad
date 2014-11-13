;;; cad.el --- produce CAD files from Lisp code

;; Copyright (C) 2014  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: convenience, hardware, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'modal-functions)

(add-to-list 'load-path (file-name-directory load-file-name))

(autoload 'gcode-mode "gcode") ; git clone https://github.com/jasapp/gcode-emacs.git
(add-to-list 'auto-mode-alist (cons ".tap" 'gcode-mode))
(eval-after-load "gcode"
  '(require 'cad-gcode))

(eval-after-load "nxml-mode"
  '(require 'cad-svg))

(eval-after-load "ps-mode"
  '(require 'cad-ps))

;;;;;;;;;;;;;;;;;;;;;;;
;; support functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defmodel cad-preamble (width height)
  "Functions to run at the start of rendering a drawing of HEIGHT and WIDTH.")

(defmodel cad-postamble ()
  "Functions to run at the end of rendering a drawing.")

;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing structure ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro drawing (name width height &body parts)
  "Define a drawing called NAME of WIDTH and HEIGHT, made of PARTS."
  `(put name 'cad-drawing
	'(progn
	   (cad-preamble)
	   ,@parts
	   (cad-postamble))))

(defmacro shape (action &body parts)
  "Define a shape on which ACTION is done, after drawing it from PARTS."
  `(progn
     (newpath)
     ,@parts
     (,action)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forms to use in drawings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro rotate (angle &body parts)
  "At ANGLE, draw PARTS."

  )

(defmacro translate (xd yd &body parts)
  "Translated by XD YD, draw PARTS."
  `(let ((xo (+ xo xd))
	 (yo (+ yo yd)))
     ,@parts))

(defmacro scale (xs ys &body parts)
  "Scaled by XS YS, draw PARTS."
  `(let ((xx (* xx xs))
	 (xy (* xy ys))
	 (yy (* yy ys))
	 (yx (* yx xs)))
     ,@parts))

(defmacro row (n step &body parts)
  "Draw N times, with a horizontal interval of STEP, the PARTS."
  `(dotimes (i n)
     (translate (* i ,step) 0
		,@parts)))

(defmacro column (n step &body parts)
  "Draw N times, with a vertical interval of STEP, the PARTS."
  `(dotimes (i n)
     (translate 0 (* i ,step)
		,@parts)))

(defmacro grid (nx ny stepx stepy &body parts)
  "Draw a grid of parts, with NX NY of them at STEPX and STEPY intervals.
PARTS are all drawn at these positions."
  )

(defmacro arcstep (n theta radius delta &body parts)
  "Draw N copies of the parts, with a first angle of THETA, RADIUS away from the current point, and a step angle of DELTA.
All of PARTS are drawn at each position."
  )

(defmacro circlestep (n theta radius delta &body parts)
  "Draw N copies of the parts, arranged in a circle.
The first one is drawn at an angle of THETA, and they are RADIUS away from the current position.
All of PARTS are drawn at each position."
  )

;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; These functions are defined separately for gcode, svg, and any
;;; other output modes.  defmodel makes a despatcher function, which
;;; is implemented by a mode-specific definition.

;; todo: line, arc, box, etc

(defmodel newpath ()
  "Begin a new path.")

(defmodel cutpath ()
  "Cut the current path.
For use as the 'action' of the `shape' macro.")

(defmodel engravepath ()
  "Engrave the current path.
For use as the 'action' of the `shape' macro.")

(defmodel fillpath ()
  "Fill the current path.
For use as the 'action' of the `shape' macro.")

(defmodel moveto (x y)
  "Move to X Y.
For use within the `shape' macro.")

(defmodel lineto (x y)
  "Draw a line from the current point to X Y.
For use within the `shape' macro.")

(defmodel arc (xc yc r ang1 ang2)
  "Draw an arc.
For use within the `shape' macro.")

(defmodel circle (r)
  "Draw a circle at the current point, of radius R.")

;;;;;;;;;;;;;;;
;; rendering ;;
;;;;;;;;;;;;;;;

(defun cad-render (symbol file)
  "Render the drawing named by SYMBOL into FILE."
  (interactive
   (let* ((drawing (completing-read "Render drawing: "
				    cad-drawings
				    nil t))
	  (file (read-file-name "Render drawing into file: ")))
     (list drawing file)))
  (let ((xx 1.0)
	(xy 0.0)
	(yy 1.0)
	(yx 0.0)
	(xo 0.0)
	(yo 0.0)
	(xc nil)
	(yc nil))
    (find-file file)
    (erase-buffer)
    (funcall (get symbol 'cad-drawing))
    (basic-save-buffer)))

(provide 'cad)
;;; cad.el ends here
