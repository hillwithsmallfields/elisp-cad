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

(autoload 'dxf-mode "dxf")
(add-to-list 'auto-mode-alist (cons ".dxf" 'dxf-mode))

(eval-after-load "nxml-mode"
  '(require 'cad-svg))

(eval-after-load "ps-mode"
  '(require 'cad-ps))

(defvar cad-use-target-transforms t
  "Whether to get the target language to do the transformations for us.
This isn't available in all languages.

Enabling this makes for better-styled code in the target language;
Disabling it is useful for debugging our own transforms.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables that get rebound ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We keep the current transformation matrix, PostScript-style, but
;;; in a collection of separate variables.  Depending on your output
;;; language, you may either want to use the user-supplied
;;; co-ordinates directly, or transformed.  If your output language
;;; has suitable operations (e.g. PostScript), you'll be using them
;;; directly, your output methods will need to output the
;;; transformation actions, using modal functions such as begin-rotate
;;; and end-rotate.  If your output language doesn't support such
;;; transformation operations (e.g. gcode), leave those functions as
;;; identity functions, and use the transformed co-ordinates.  Even
;;; better, provide both possibilities, using
;;; cad-use-target-transforms to choose between them.

(defvar xc nil
  "The x part of the cursor.")

(defvar yc nil
  "The y part of the cursor.")

(defvar xx 1.0
  "The x (in) contribution to the x (out).")

(defvar yx 0.0
  "The y (in) contribution to the x (out).")

(defvar yy 1.0
  "The y (in) contribution to the y (out).")

(defvar xy 0.0
  "The x (in) contribution to the y (out).")

(defvar xo 0.0
  "The x offset.")

(defvar yo 0.0
  "The y offset.")

(defvar action 'cutpath
  "The action to apply to the current path.

This is set at the start of drawing the shape, so that it's
available for output languages that use it as they go along.

It is also used as the action at the end of creating a shape.")

(defvar default-action 'cutpath
  "The default action, for shapes that inherit their action.
They can override this by defining their own action.")

(defvar cad-colour "black"
  "The current colour.")

(defvar cad-canvas-width nil
  "The width of the canvas.")

(defvar cad-canvas-height nil
  "The height of the canvas.")

;;;;;;;;;;;;;;;;;;;;;;;
;; support functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defmodel cad-preamble (width height)
  "Functions to run at the start of rendering a drawing of HEIGHT and WIDTH.")

(defmodel cad-postamble ()
  "Functions to run at the end of rendering a drawing.")

(defun tx (rx ry)
  "Return the X part of the transformed form of RX RY."
  (+ (* rx xx) (* ry yx) xo))

(defun ty (rx ry)
  "Return the Y part of the transformed form of RX RY."
  (+ (* rx xy) (* ry yy) yo))

;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing structure ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar cad-drawings nil
  "List of symbols naming defined drawings.
The drawing is held on the 'cad-drawing property of each symbol.")

(defmacro drawing (name action width height &rest parts)
  "Define a drawing called NAME using ACTION of WIDTH and HEIGHT, made of PARTS."
  `(progn
     (put ',name 'cad-drawing
	  '(let ((default-action ',action)
		 (cad-canvas-width ,width)
		 (cad-canvas-height ,height))
	     (cad-preamble ,width ,height)
	     ,@parts
	     (cad-postamble)))
     (add-to-list 'cad-drawings '(,name))))

(defmacro shape (&rest parts)
  "Define a shape made of PARTS and draw it.
If the first argument is a symbol, it is used as the drawing action."
  (if (symbolp (car parts))
      `(let ((action ',(car parts)))
	 (newpath)
	 ,@(cdr parts)
	 (,(car parts)))
    `(let ((action default-action))
       (newpath)
       ,@parts
       (funcall default-action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forms to use in drawings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro rotate (angle &rest parts)
  "Rotated by ANGLE, draw PARTS."
  `(let* ((rad (degrees-to-radians ,angle))
	 (as (sin rad))
	 (ac (cos rad)))
    (let ((xx (+ (* xx ac) (* yx as)))
	  (xy (+ (* xy ac) (* yy as)))
	  (yx (+ (* yx ac) (* xx as)))
	  (yy (+ (* yy ac) (* xy as))))
      (begin-rotate rad ,angle)
      ,@parts
      (end-rotate))))

(defmacro translate (xd yd &rest parts)
  "Translated by XD YD, draw PARTS."
  `(let ((xo (+ xo xd))
	 (yo (+ yo yd)))
     (begin-translate xd yd)
     ,@parts
     (end-translate)))

(defmacro scale (xs ys &rest parts)
  "Scaled by XS YS, draw PARTS."
  `(let ((xx (* xx xs))
	 (xy (* xy ys))
	 (yy (* yy ys))
	 (yx (* yx xs)))
     (begin-scale xs ys)
     ,@parts
     (end-scale)))

(defmacro colour (col &rest parts)
  "In COL, draw PARTS."
  `(let ((cad-colour ,col))
     (begin-colour ,col)
     ,@parts
     (end-colour)))

(defmacro row (n step &rest parts)
  "Draw N times, with a horizontal interval of STEP, the PARTS."
  `(dotimes (i n)
     (translate (* i ,step) 0
		,@parts)))

(defmacro column (n step &rest parts)
  "Draw N times, with a vertical interval of STEP, the PARTS."
  `(dotimes (i n)
     (translate 0 (* i ,step)
		,@parts)))

(defmacro grid (nx ny stepx stepy &rest parts)
  "Draw a grid of parts, with NX NY of them at STEPX and STEPY intervals.
PARTS are all drawn at these positions."
  )

(defmacro arcstep (n theta radius delta &rest parts)
  "Draw N copies of the parts, with a first angle of THETA, RADIUS away from the current point, and a step angle of DELTA.
All of PARTS are drawn at each position."
  )

(defmacro circlestep (n theta radius delta &rest parts)
  "Draw N copies of the parts, arranged in a circle.
The first one is drawn at an angle of THETA, and they are RADIUS away from the current position.
All of PARTS are drawn at each position."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformation support functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These functions are defined separately for gcode, svg, and any
;;; other output modes.  defmodel makes a despatcher function, which
;;; is implemented by a mode-specific definition.

(defmodel begin-rotate (rad deg)
  "Begin rotating by RAD or DEG.")

(defmodel end-rotate ()
  "End a rotation.")

(defmodel begin-translate (xd yd)
  "Begin a translation by XD YD.")

(defmodel end-translate ()
  "End a translation.")

(defmodel begin-scale (xs ys)
  "Begin scaling by XS YS.")

(defmodel end-scale ()
  "End scaling.")

(defmodel begin-colour (colour)
  "Begin using COLOUR.")

(defmodel end-colour ()
  "End using a colour.")

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

(defmodel circle (r &optional label)
  "Draw a circle at the current point, of radius R.
An optional LABEL may be given.")

(defmodel rectangle (w h &optional label)
  "Draw a rectangle at the current point, of W and H.
An optional LABEL may be given.
The bottom left corner is at the current point.")

(defmodel arc (cx cy r a1 a2 &optional label)
  "Draw an arc centred at CX CY of radius R between angles A1 and A2.")

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
    (eval (get (intern symbol) 'cad-drawing))
    (basic-save-buffer)))

(provide 'cad)
;;; cad.el ends here
