;;; cad-ps.el --- ps drawing functions for elisp-cad

;; Copyright (C) 2014  John Sturdy

;; Author: John Sturdy <john.sturdy@arm.com>
;; Keywords: multimedia

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

(defmodal cad-preamble ps-mode (width height)
  (insert "%!PS\n")
  (ident-matrix))

(defmodal cad-postamble ps-mode ()
  (insert "showpage\n"))

(defmodal begin-rotate ps-mode (rad deg)
  (insert (format "gsave\n%f rotate\n" deg)))

(defmodal end-rotate ps-mode ()
  (insert "grestore\n"))

(defmodal begin-translate ps-mode (xd yd)
  (insert (format "gsave\n%f %f translate\n" xd yd)))

(defmodal end-translate ps-mode ()
  (insert "grestore\n"))

(defmodal begin-scale ps-mode (xs ys)
  (insert (format "gsave\n%f %f scale\n" xd yd)))

(defmodal end-scale ps-mode ()
  (insert "grestore\n"))

(defmodal begin-colour ps-mode (colour)
  ;; todo: convert the colour to PS format
  )

(defmodal end-colour ps-mode ())

(defmodal newpath ps-mode ()
  (insert "newpath\n"))

(defmodal cutpath ps-mode ())

(defmodal engravepath ps-mode ())

(defmodal fillpath ps-mode ())

(defmodal moveto ps-mode (x y)
  (insert (format "%f %f moveto\n" x y)))

(defmodal lineto ps-mode (x y)
    (insert (format "%f %f lineto\n" x y)))

(defmodal cad-circle ps-mode (r))

(defmodal cad-rectangle ps-mode (left bottom width height &optional label)
  (let ((top (+ bottom height))
	(right (+ left width)))
    (insert
     (format "%f %f moveto %f %f lineto %f %f lineto %f %f lineto closepath\n"
	     left bottom left top right top right bottom)))))

(defmodal cad-rounded-rectangle ps-mode (left bottom width height radius &optional label)
  (let* ((top (+ bottom height))
	 (right (+ left width))
	 (left2 (+ left radius))
	 (bottom2 (+ bottom radius))
	 (right2 (- right radius))
	 (top2 (- top radius)))
    (insert
     (format "")
     )))

(defmodal cad-arc ps-mode (cx cy r a1 a2 &optional label))

(provide 'cad-ps)

;;; cad-ps.el ends here
