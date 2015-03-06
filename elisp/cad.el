;;; cad.el --- produce CAD files from Lisp code

;; Copyright (C) 2014, 2015  John Sturdy

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

;; todo: fill in top-of etc and add height-of and width-of --- must think about handling coordinate system changes, probably have to keep them in absolute coordinates
;; todo: allow coordinate pairs/triplets
;; todo: add top-left-corner-of etc

;;; Code:

(add-to-list 'load-path (file-name-directory load-file-name))

(require 'modal-functions)

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

;;; We keep a current transformation matrix, PostScript-style.
;;; Depending on your output language, you may either want to use the
;;; user-supplied co-ordinates directly, or transformed.  If your
;;; output language has suitable operations (e.g. PostScript), you'll
;;; be using them directly, your output methods will need to output
;;; the transformation actions, using modal functions such as
;;; begin-rotate and end-rotate.  If your output language doesn't
;;; support such transformation operations (e.g. gcode), leave those
;;; functions as identity functions, and use the transformed
;;; co-ordinates.  Even better, provide both possibilities, using
;;; cad-use-target-transforms to choose between them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transformation matrix functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ident-matrix ()
  "Return a fresh PostScript identity matrix."
  (vector 1 0 0 1 0 0))

(defmacro matrix-aref (matrix y x)
  "Convert a ps-matrix-style reference into a ps-vector-style one."
  `(aref ,matrix (+ (* ,y 2) ,x)))

(defun mult-matrix (m1 m2)
  "Matrix multiply M1 by M2."
  (let ((newm (vector (+ (* (matrix-aref m1 0 0) (matrix-aref m2 0 0))
			 (* (matrix-aref m1 0 1) (matrix-aref m2 1 0)))
		      (+ (* (matrix-aref m1 0 0) (matrix-aref m2 0 1))
			 (* (matrix-aref m1 0 1) (matrix-aref m2 1 1)))
		      (+ (* (matrix-aref m1 1 0) (matrix-aref m2 0 0))
			 (* (matrix-aref m1 1 1) (matrix-aref m2 1 0)))
		      (+ (* (matrix-aref m1 1 0) (matrix-aref m2 0 1))
			 (* (matrix-aref m1 1 1) (matrix-aref m2 1 1)))
		      (+ (* (matrix-aref m1 2 0) (matrix-aref m2 0 0))
			 (* (matrix-aref m1 2 1) (matrix-aref m2 1 0)))
		      (+ (* (matrix-aref m1 2 0) (matrix-aref m2 0 1))
			 (* (matrix-aref m1 2 1) (matrix-aref m2 1 1)))
		      )))
    (aset newm 4 (+ (aref newm 4) (matrix-aref m2 2 0)))
    (aset newm 5 (+ (aref newm 5) (matrix-aref m2 2 1)))
    newm))

(defun translate-matrix (m x y)
  "To matrix M add a translation of X Y."
  (mult-matrix (vector 1 0 0 1 x y) m))

(defun scale-matrix (m x y)
  "To matrix M add a scaling of X Y."
  (mult-matrix (vector x 0 0 y 0 0) m))

(defun rotate-matrix (m a)
  "To matrix M add a rotation of A."
  (let ((thecos (cos (degrees-to-radians a)))
	(thesin (sin (degrees-to-radians a))))
    (mult-matrix (vector thecos thesin (- thesin) thecos 0 0) m)))

(defmacro xmtransform (x y m)
  "The X part of the result of transforming X Y by the matrix M."
  `(+ (* ,x (aref ,m 0))
      (* ,y (aref ,m 2))
      (aref ,m 4)))

(defmacro ymtransform (x y m)
  "The Y part of the result of transforming X Y by the matrix M."
  `(+ (* ,x (aref ,m 1))
      (* ,y (aref ,m 3))
      (aref ,m 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables that get rebound ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ctm (ident-matrix))

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
  ;; todo: setting of units / scale
  "Function to run at the start of rendering a drawing of HEIGHT and WIDTH.
Must return a suitable transformation matrix for the output device.")

(defmodel cad-postamble ()
  "Function to run at the end of rendering a drawing.")

(defun tx (rx ry)
  "Return the X part of the transformed form of RX RY."
  (xmtransform rx ry ctm))

(defun ty (rx ry)
  "Return the Y part of the transformed form of RX RY."
  (ymtransform rx ry ctm))

(defun cad-get-parameter (proplist name)
  "From PROPLIST get the parameter called NAME."
  (plist-get proplist name))

(defun cad-parameter (proplist name &rest other-ways)
  "From PROPLIST get the parameter called NAME, or deduce it.
OTHER-WAYS is a list of ways of deducing it."
  (or (plist-get proplist name)
      (catch 'done
	;; (message "Trying to deduce %S from %S" name proplist)
	(dolist (this-way other-ways)
	  (let ((other-op (car this-way))
		(other-args (cdr this-way)))
	    ;; (message "Trying %S: other-op=%S other-args=%S" this-way other-op other-args)
	    (let ((argvals (mapcar (lambda (other-prop)
				     (message "Trying other-prop=%S" other-prop)
				     (if (symbolp other-prop)
					 (eval (plist-get proplist other-prop))
				       other-prop))
				   other-args)))
	      ;; (message "argvals=%S; with and = %S" argvals (list 'and argvals))
	      (when (eval (cons 'and argvals)) ; i.e. all non-nil; can't use `apply' on `and' as it's a special forma
		;; (message "Got usable combination")
		(throw 'done (apply other-op argvals))))))
	(message "No suitable combination of parameters was given: need any of %s to be found in %S"
	       (mapconcat (lambda (descr)
			    (format "%s and %s" (cadr descr) (caddr descr)))
			  other-ways ", or ")
	       proplist)
	nil)))

(defun fill-in (if-given op &rest opargs)
  "If IF-GIVEN is given, return it, otherwise apply OP to OPARGS."
  (message "fill-in %S (%S %S)" if-given op opargs)
  (or if-given
      (apply op opargs)))

(defun -/2 (from difference)
  "Return the result of starting at FROM and removing half the DIFFERENCE.
For generating an edge co-ordinate from the centre and width or height."
  (- from (/ difference 2.0)))

(defun *2- (a b)
  "Return twice the result of from A subtracting B."
  (* 2 (- a b)))

(defun mid-point (a b)
  "Return the mid-point of A and B."
  (+ a (/ (- b a) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Co-ordinate pairs/triplets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xy-point (x y)
  "Make an x-y point."
  (vector x y))

(defun xyz-point (x y z)
  "Make an x-y-z point."
  (vector x y z))

(defun x-coord (v)
  "Return the x part of vector V."
  (aref v 0))

(defun y-coord (v)
  "Return the y part of vector V."
  (aref v 1))

(defun z-coord (v)
  "Return the z part of vector V."
  (aref v 2))

(defun x-distance (v1 v2)
  "Return the horizontal distance between V1 and V2."
  (- (x-coord v2) (x-coord v1)))

(defun y-distance (v1 v2)
  "Return the vertical distance between V1 and V2."
  (- (y-coord v2) (y-coord v1)))

(defun z-distance (v1 v2)
  "Return the out-of-plane distance between V1 and V2."
  (- (z-coord v2) (z-coord v1)))

(defun distance (v1 v2)
  "Return the distance between V1 and V2."
  (cond
   ((and (numberp v1) (numberp v2))
    (- v2 v1))
   ((and (vectorp v1) (= (length v1) 2)
	 (vectorp v2) (= (length v2) 2))
    (sqrt (+ (expt (x-distance v1 v2) 2.0)
	     (expt (y-distance v1 v2) 2.0))))
   ((and (vectorp v1) (= (length v1) 3)
	 (vectorp v2) (= (length v2) 3))
    (expt (+ (expt (x-distance v1 v2))
	     (expt (y-distance v1 v2))
	     (expt (z-distance v1 v2))) -3))
   (t (error "Cannot calculate distance between %S and %S"))))

;;;;;;;;;;;;;;;;;;;;;
;; Naming elements ;;
;;;;;;;;;;;;;;;;;;;;;

(defvar cad-symbols nil
  "List of symbols on which we have set a cad property.")

(defun cad-set-properties (name &rest props)
  "Indicate that NAME has PROPERTIES."
  (add-to-list 'cad-symbols name)
  (put name 'elisp-cad-props props))

(defun cad-get-properties (name)
  "Return the cad properties of NAME."
  (get name 'elisp-cad-props))

(defun cad-list-symbols ()
  "Display the symbols we have set properties on, with their properties."
  (interactive)
  (with-output-to-temp-buffer "*CAD properties*"
    (dolist (symbol cad-symbols)
      (let ((properties (cad-get-properties symbol)))
	(princ (format "%s:\n" symbol))
	(while properties
	  (princ (format "  %s: %S\n" (car properties) (cadr properties)))
	  (setq properties (cddr properties)))))))

(defun cad-set-property (name key value)
  "Indicate that for NAME, KEY has VALUE."
  (add-to-list 'cad-symbols name)
  (put name 'elisp-cad-props
       (plist-put (plist-get name 'elisp-cad-props)
		  key value)))

(defun cad-set-edges (name left bottom right top)
  "Declare that NAME has LEFT BOTTOM RIGHT TOP."
  (cad-set-property name 'left left)
  (cad-set-property name 'bottom bottom)
  (cad-set-property name 'right right)
  (cad-set-property name 'top top))

(defun cad-property (name property)
  (plist-get (get name 'elisp-cad-props) property))

(defmacro top-of (eltname)
  "Return the top of the element called ELTNAME"
  `(cad-property ',eltname 'top))

(defmacro bottom-of (eltname)
  "Return the bottom of the element called ELTNAME"
  `(cad-property ',eltname 'bottom))

(defmacro left-of (eltname)
  "Return the left of the element called ELTNAME"
  `(cad-property ',eltname 'left))

(defmacro right-of (eltname)
  "Return the right of the element called ELTNAME"
  `(cad-property ',eltname 'right))

(defmacro top-left-of (eltname)
  "Return the top left of the element called ELTNAME"
  `(xy-point (cad-property ',eltname 'left)
	     (cad-property ',eltname 'top)))

(defmacro top-right-of (eltname)
  "Return the top right of the element called ELTNAME"
  `(xy-point (cad-property ',eltname 'right)
	     (cad-property ',eltname 'top)))

(defmacro bottom-left-of (eltname)
  "Return the bottom left of the element called ELTNAME"
  `(xy-point (cad-property ',eltname 'left)
	     (cad-property ',eltname 'bottom)))

(defmacro bottom-right-of (eltname)
  "Return the bottom right of the element called ELTNAME"
  `(xy-point (cad-property ',eltname 'right)
	     (cad-property ',eltname 'bottom)))

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
	  '(let* ((default-action ',action)
		 (cad-canvas-width ,width)
		 (cad-canvas-height ,height)
		 (ctm (cad-preamble ,width ,height)))
	     ,@parts
	     (cad-postamble)))
     (add-to-list 'cad-drawings '(,name))))

(defun cad-display-drawing (symbol)
  "Display the drawing attached to SYMBOL."
  (interactive (list (intern (completing-read "Display drawing source: " cad-drawings nil t))))
  (let ((drawing-code (get symbol 'cad-drawing)))
    (with-output-to-temp-buffer (format "*drawing %S*" symbol)
      (pp drawing-code))))

(defun non-string-car (l)
  "Return the car of L, unless it is a string, in which case return the cadr."
  (if (stringp (car l))
      (cadr l)
    (car l)))

(defun non-string-cdr (l)
  "Return the cdr of L, unless it is a string, in which case return the cadr."
  (if (stringp (car l))
      (cddr l)
    (cdr l)))

(defun non-string-identity (l)
  "Return L, unless its car is a string, in which return its cdr."
  (if (stringp (car l))
      (cdr l)
    l))

(defmacro shape (&rest parts)
  "Define a shape made of PARTS and draw it.
If the first argument is a symbol, it is used as the drawing action.
If the first or second argument is a string, it is used to name the shape."
  ;; todo: implement the naming; so far, I only filter it out
  (if (symbolp (non-string-car parts))
      `(let ((action ',(non-string-car parts)))
	 (newpath)
	 ,@(non-string-cdr parts)
	 (,(non-string-car parts))
	 (funcall action))
    `(let ((action default-action))
       (newpath)
       ,@(non-string-identity parts)
       (funcall action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forms to use in drawings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro rotate (angle &rest parts)
  "Rotated by ANGLE, draw PARTS."
  `(let ((rad (degrees-to-radians ,angle))
	 (ctm (rotate-matrix ctm ,angle)))
     (begin-rotate rad ,angle)
     ,@parts
     (end-rotate)))

(defmacro translate (xd yd &rest parts)
  "Translated by XD YD, draw PARTS."
  `(let ((ctm (translate-matrix ctm ,xd ,yd)))
     (begin-translate ,xd ,yd)
     ,@parts
     (end-translate)))

(defmacro scale (xs ys &rest parts)
  "Scaled by XS YS, draw PARTS."
  `(let ((ctm (scale-matrix m ,xs ,ys)))
     (begin-scale ,xs ,ys)
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

(defmodel moveto-xy (x y)
  "Move to X Y.
For use within the `shape' macro.")

(defmacro apply-to-parameter-pair (function parameters)
  "Apply FUNCTION to the first two points from PARAMETERS."
  (let ((first-param (car parameters)))
    (if (vectorp first-param)
	`'(progn
	   (,function ,(aref first-param 0) ,(aref first-param 1))
	   ',(cdr parameters))
      `'(progn
	 (,function ,first-param ,(cadr parameters))
	 ',(cddr parameters)))))

(defmacro moveto (&rest parameters)
  "Keyworded macro for moving to a point using PARAMETERS."
  ;; todo: set the edges?
  `(apply-to-parameter-pair moveto-xy ,parameters))

(defmodel lineto-xy (x y)
  "Draw a line from the current point to X Y.
For use within the `shape' macro.")

(defmacro lineto (&rest parameters)
  "Keyworded macro for drawing a line to a point using PARAMETERS."
  ;; todo: set the edges
  `(apply-to-parameter-pair lineto-xy ,parameters))

(defmodel cad-circle (r &optional label)
  "Draw a circle at the current point, of radius R.
An optional LABEL may be given.")

(defmacro circle (&rest parameters)
  "Keyworded macro for circle drawing using PARAMETERS."
  ;; todo: allow name parameter
  (let* ((centre-x (cad-parameter parameters 'centre-x
				  '(mid-point left right)))
	 (centre-y (cad-parameter parameters 'centre-y
				  '(mid-point bottom top)))
	 (radius (cad-parameter parameters 'radius
				'(-/2 top bottom)
				'(-/2 right left)))
	 (label (cad-get-parameter parameters 'name)))
    ;; todo: finish this, using two possible deduction types
    ;; todo: set the edges
    ))

(defmodel cad-rectangle (left bottom width height &optional label)
  "Draw a rectangle at LEFT BOTTOM, of WIDTH and HEIGHT.
An optional LABEL may be given.")

(defmacro rectangle (&rest parameters)
  "Keyworded macro for rectangle drawing using PARAMETERS."
  ;; todo: ? make sure parameter evaluators can access already-found parameters --- should be OK given that elisp has dynamic scope?
  (let* ((bottom (cad-parameter parameters 'bottom
				'(- top height)
				'(-/2 y-centre height)
				'(y-coord bottom-left-corner)
				'(y-coord bottom-right-corner)))
	 (height (cad-parameter parameters 'height
				'(- top bottom)
				'(*2- y-centre bottom)
				'(*2- top y-centre)
				'(y-distance top-left-corner bottom-left-corner)
				'(y-distance top-left-corner bottom-right-corner)
				'(y-distance top-right-corner bottom-left-corner)
				'(y-distance top-right-corner bottom-right-corner)))
	 (left (cad-parameter parameters 'left
			      '(- right width)
			      '(-/2 x-centre width)
			      '(x-coord bottom-left-corner)
			      '(x-coord top-left-corner)))
	 (width (cad-parameter parameters 'width
			       '(- right left)
			       '(*2- right x-centre)
			       '(*2- x-centre left)
			       '(x-distance top-right-corner top-left-corner)
			       '(x-distance top-right-corner bottom-left-corner)
			       '(x-distance bottom-right-corner top-left-corner)
			       '(x-distance bottom-right-corner bottom-left-corner)))
	 (top (cad-parameter parameters 'top
			     '(+ bottom height)
			     '(y-coord top-left-corner)
			     '(y-coord top-right-corner)))
	 (right (cad-parameter parameters 'right
			       '(+ left width)
			       '(x-coord bottom-right-corner)
			       '(x-coord top-right-corner)))
	 (label (cad-get-parameter parameters 'name)))
    (if label
	`(progn
	   (setq top (fill-in ,top '+ ,bottom ,height)
		 right (fill-in ,right '+ ,left ,width))
	   (cad-set-edges ',label ,left ,bottom ,right ,top)
	   (cad-rectangle ,left ,bottom ,width ,height ',label))
      `(progn
	 (cad-rectangle ,left ,bottom ,width ,height)))))

(defmodel cad-rounded-rectangle (left bottom width height radius &optional label)
  "Draw a rounded rectangle at LEFT BOTTOM, of WIDTH and HEIGHT and corner RADIUS.
An optional LABEL may be given.")

(defmacro rounded-rectangle (&rest parameters)
  "Keyworded macro for rounded rectangle drawing using PARAMETERS."
  (let* ((bottom (cad-parameter parameters 'bottom
				'(- top height)
				'(-/2 y-centre height)
				'(y-coord bottom-left-corner)
				'(y-coord bottom-right-corner)))
	 (height (cad-parameter parameters 'height
				'(- top bottom)
				'(*2- y-centre bottom)
				'(*2- top y-centre)
				'(y-distance top-left-corner bottom-left-corner)
				'(y-distance top-left-corner bottom-right-corner)
				'(y-distance top-right-corner bottom-left-corner)
				'(y-distance top-right-corner bottom-right-corner)))
	 (left (cad-parameter parameters 'left
			      '(- right width)
			      '(-/2 x-centre width)
			      '(x-coord bottom-left-corner)
			      '(x-coord top-left-corner)))
	 (width (cad-parameter parameters 'width
			       '(- right left)
			       '(*2- right x-centre)
			       '(*2- x-centre left)
			       '(x-distance top-right-corner top-left-corner)
			       '(x-distance top-right-corner bottom-left-corner)
			       '(x-distance bottom-right-corner top-left-corner)
			       '(x-distance bottom-right-corner bottom-left-corner)))
	 (top (cad-parameter parameters 'top
			     '(+ bottom height)
			     '(y-coord top-left-corner)
			     '(y-coord top-right-corner)))
	 (right (cad-parameter parameters 'right
			       '(+ left width)
			       '(x-coord bottom-right-corner)
			       '(x-coord top-right-corner)))
	 (radius (cad-get-parameter parameters 'radius))
	 (label (cad-get-parameter parameters 'name)))
    (message "in expander, bottom=%S height=%S left=%S width=%S top=%S right=%S radius=%S label=%S" bottom height left width top right radius label)
    (if label
	`(progn
           (setq top (fill-in ,top '+ ,bottom ,height)
		 right (fill-in ,right '+ ,left ,width))
	   (cad-set-edges ',label ,left ,bottom ,right ,top)
	   (cad-rounded-rectangle ,left ,bottom ,width ,height ,radius ',label))
      `(progn
	 (cad-rounded-rectangle ,left ,bottom ,width ,height ,radius)))))

(defmodel cad-arc (cx cy r a1 a2 &optional label)
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
