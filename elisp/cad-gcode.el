;;; cad-gcode.el --- gcode drawing functions for elisp-cad

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

;; See http://gnipsel.com/linuxcnc/index.html for tutorial

;;; Code:

(defmodal cad-preamble gcode-mode (width height)
  (insert (format "; drawing with width=%d and height=%d\n" width height))
  (ident-matrix))

(defmodal cad-postamble gcode-mode ()
  )

(defmodal begin-rotate gcode-mode (rad deg))

(defmodal end-rotate gcode-mode ())

(defmodal begin-translate gcode-mode (xd yd))

(defmodal end-translate gcode-mode ())

(defmodal begin-scale gcode-mode (xs ys))

(defmodal end-scale gcode-mode ())

(defmodal begin-colour gcode-mode (colour))

(defmodal end-colour gcode-mode ())

(defmodal newpath gcode-mode ())

(defmodal cutpath gcode-mode ())

(defmodal engravepath gcode-mode ())

(defmodal fillpath gcode-mode ())

(defmodal moveto gcode-mode (x y)
  ;; todo: vertical move first as needed
  (insert (format "G0 X%f Y%f Z0\n" x y)))

(defmodal lineto gcode-mode (x y)
  ;; todo: vertical move first as needed
  (insert (format "G1 X%f Y%f Z0\n" x y)))

(defmodal cad-circle gcode-mode (r))

(defmodal cad-rectangle gcode-mode (l b w h &optional label))

(defmodal cad-rounded-rectangle gcode-mode (l b w h r &optional label))

(defmodal cad-arc gcode-mode (cx cy r a1 a2 &optional label))

(provide 'cad-gcode)

;;; cad-gcode.el ends here
