;;; cad-svg.el --- svg drawing functions for elisp-cad

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

(defmodal cad-preamble nxml-mode (width height)
  (insert  (format "<svg width=\"%d\" height=\"%d\">\n" width height)))

(defmodal cad-postamble nxml-mode ()
  (insert "</svg>\n"))

(defmodal newpath nxml-mode ())

(defmodal cutpath nxml-mode ())

(defmodal engravepath nxml-mode ())

(defmodal fillpath nxml-mode ())

(defmodal moveto nxml-mode (x y)
  (setq xc x yc y))

(defmodal lineto nxml-mode (x y))

(defmodal arc nxml-mode (xc yc r ang1 ang2))

(defmodal circle nxml-mode (r)
  (insert (format "<circle cx=\"%f\" cy=\"%f\" r=\"%f\"/>\n" xc yc r)))

(provide 'cad-svg)

;;; cad-svg.el ends here
