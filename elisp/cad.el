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

;;;;;;;;;;;;;;;;;;;;;;;
;; support functions ;;
;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-level of each drawing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro drawing (name &body parts)
  "Define a drawing called NAME, made of PARTS."
  )

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

;; todo: line, arc, box, etc

;;;;;;;;;;;;;;;
;; rendering ;;
;;;;;;;;;;;;;;;

(defun cad-render (symbol)
  "Render the drawing named by SYMBOL."
   (let ((xx 1.0)
	 (xy 0.0)
	 (yy 1.0)
	 (yx 0.0)
	 (xo 0.0)
	 (yo 0.0))
))

(provide 'cad)
;;; cad.el ends here
