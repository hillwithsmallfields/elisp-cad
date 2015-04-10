;;; cad-example.el --- examples and tests for cad.el

;; Copyright (C) 2014, 2015  John Sturdy

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

(drawing just-a-circle cutpath 400 400 pixels
	 (shape (moveto 200 200)
		(cad-circle 100)))

(drawing just-a-rectangle cutpath 400 400 pixels
	 (shape (moveto 200 200)
		(cad-rectangle 150 100)))

(drawing fancy-rectangle cutpath 400 400 pixels
	 (rectangle width 200 height 200 left 100 top 300))

(drawing stacked-rectangles cutpath 800 1200 pixels
	 (shape (rectangle left 100 bottom 100 height 200 width 600)
		(rectangle left 150 bottom 300 height 100 width 500)
		(rectangle left 200 bottom 400 height 50 width 400)))

(drawing tilted-rectangle cutpath 400 400 pixels
	 (shape (rotate 15
			(moveto 200 200)
			(cad-rectangle 150 100 "tilted"))))

(drawing arc-fragment cutpath 400 400 pixels
	 (shape (moveto 200 200)
		(cad-circle 50)
		(cad-arc 200 200 100 0 60)))

(drawing many-arcs cutpath 1400 1400 pixels
	 (shape (rectangle bottom 10 left 10 width 200 height 20))
	 (dotimes (i 12)
	   (dotimes (j 12)
	     (shape (cad-arc (* j 50) (* i 50) 12 (* j 30) (* i 30))))))

(drawing rounded-rectangle-test cutpath 600 600 pixels
	 (shape
	  (rounded-rectangle bottom 100 left 100 top 300 right 400 radius 25)))

(drawing phablet-surround cutpath 300 300 millimetres
	 (let* ((frame-width 230)
		(frame-height 130)
		(frame-x-centre (/ frame-width 2))
		(frame-y-centre (/ frame-height 2))
		(tablet-width 188)
		(tablet-height 109)
		(screen-width 156)
		(screen-height 88)
		(frame-side (/ (- frame-width tablet-width) 2))
		(frame-bottom (/ (- frame-height tablet-height) 2))
		(bezel-side (/ (- frame-width screen-width) 2))
		(bezel-bottom (/ (- frame-height screen-height) 2)))
	   (translate 10 10
		      (shape "tablet-frame"
			     (rounded-rectangle x-centre frame-x-centre
						y-centre frame-y-centre
						width frame-width
						height frame-height
						radius 10)
			     (rounded-rectangle name hole-for-tablet
						x-centre frame-x-centre
						y-centre frame-y-centre
						width tablet-width
						height tablet-height
						radius 10)
			     (rectangle left 67 
					top (bottom-of hole-for-tablet)
					width 10 height 10)
			     ))
	   (translate 10 150
		      (shape "tablet-bezel"
			     (rounded-rectangle x-centre frame-x-centre
						y-centre frame-y-centre
						width frame-width
						height frame-height
						radius 10)
			     (rounded-rectangle x-centre frame-x-centre
						y-centre frame-y-centre
						width screen-width
						height screen-height
						radius 10)))))

(provide 'cad-example)
;;; cad-example.el ends here
