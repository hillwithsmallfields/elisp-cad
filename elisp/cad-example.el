;;; cad-example.el --- examples and tests for cad.el

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

(require 'cad)

(drawing just-a-circle cutpath 400 400
	 (shape
	  (moveto 200 200)
	  (circle 100)))

(drawing just-a-rectangle cutpath 400 400
	 (shape
	  (moveto 200 200)
	  (rectangle 150 100)))

(drawing tilted-rectangle cutpath 400 400
	 (shape
	  (rotate 15
		  (moveto 200 200)
		  (rectangle 150 100))))

(provide 'cad-example)
;;; cad-example.el ends here
