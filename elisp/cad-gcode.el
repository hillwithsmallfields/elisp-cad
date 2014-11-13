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

;; 

;;; Code:

(defmodal cad-preamble gcode-mode (width height)
  )

(defmodal cad-postamble gcode-mode ()
  )

(defmodal newpath gcode-mode ())

(defmodal cutpath gcode-mode ())

(defmodal engravepath gcode-mode ())

(defmodal fillpath gcode-mode ())

(defmodal moveto gcode-mode (x y))

(defmodal lineto gcode-mode (x y))

(defmodal arc gcode-mode (xc yc r ang1 ang2))

(provide 'cad-svg)

;;; cad-gcode.el ends here
