elisp-cad
=========

Emacs-lisp functions for creating CAD files

This package lets you design two-dimensional shapes by running pieces of Lisp.

The plan is to have several back ends, including GCODE and SVG.  The
SVG output will integrate with Emacs' SVG display; but the main aim is
to produce GCODE directly, sidestepping CNC machines' shoddy
conversion programs.

Using elisp-cad
---------------

Each drawing must be defined in a form

  (drawing name height width
      (shape cutshape ...)
      (shape fillshape ...))

All drawing must be done within a `shape' form, and shape forms must
be within a `drawing' form.

A drawing can be rendered using the command cad-render, which will
output the drawing to a file, in the appropriate format for that
filename.

