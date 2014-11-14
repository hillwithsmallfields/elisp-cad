;;; cad-shape-library.el --- shapes for cad.el

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

;; This defines some common non-primitive shapes for use in drawings
;; that use cad.el.

;;; Code:

(defun rounded-rectangle (width height radius)
  "Draw a rounded rectangle of WIDTH HEIGHT, with rounding of RADIUS.")

(provide 'cad-shape-library)
;;; cad-shape-library.el ends here
