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
  (insert "%!PS\n"))

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

(defmodal begin-colour ps-mode (colour))

(defmodal end-colour ps-mode)

(defmodal newpath ps-mode ())

(defmodal cutpath ps-mode ())

(defmodal engravepath ps-mode ())

(defmodal fillpath ps-mode ())

(defmodal moveto ps-mode (x y))

(defmodal lineto ps-mode (x y))

(defmodal arc ps-mode (xc yc r ang1 ang2))

(defmodal circle ps-mode (r))

(defmodal rectangle ps-mode (w h))

(provide 'cad-ps)

;;; cad-ps.el ends here
