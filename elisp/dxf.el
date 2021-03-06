;;; dxf.el --- editing mode for dxf files

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

;; Reference for DXF is at http://images.autodesk.com/adsk/files/acad_dxf0.pdf

;;; Code:

(define-derived-mode dxf-mode fundamental-mode "DXF"
  "Simple mode for DXF files (drawing exchange).
In the initial version, this is just a renaming of
`fundamental-mode', for use with cad.el, which uses mode-specific
definitions to determine the ouput it creates.")

(provide 'dxf)

;;; dxf.el ends here
