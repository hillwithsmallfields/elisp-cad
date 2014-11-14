;;; cad-dxf.el --- dxf drawing functions for elisp-cad

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

(defmodal cad-preamble dxf-mode (width height)
  (error "Drawing to dxf not yet implemented"))

(defmodal cad-postamble dxf-mode ())

(defmodal begin-rotate dxf-mode (rad deg))

(defmodal end-rotate dxf-mode ())

(defmodal begin-translate dxf-mode (xd yd))

(defmodal end-translate dxf-mode ())

(defmodal begin-scale dxf-mode (xs ys))

(defmodal end-scale dxf-mode ())

(defmodal begin-colour dxf-mode (colour))

(defmodal end-colour dxf-mode ())

(defmodal newpath dxf-mode ())

(defmodal cutpath dxf-mode ())

(defmodal engravepath dxf-mode ())

(defmodal fillpath dxf-mode ())

(defmodal moveto dxf-mode (x y))

(defmodal lineto dxf-mode (x y))

(defmodal arc dxf-mode (xc yc r ang1 ang2))

(defmodal circle dxf-mode (r))

(defmodal rectangle dxf-mode (w h))

(provide 'cad-dxf)

;;; cad-dxf.el ends here
