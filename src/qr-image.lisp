;; Copyright (C) 2010, 2011 johnc <jnjcc@live.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, see <http://www.gnu.org/licenses/>.

;;;; qr-image.lisp
;;;; This file contains utilities to encode symbol into images.
;;;; For now, only png format, using zpng library

(in-package #:cl-qrencode)

(defun set-color (pngarray x y color)
  (setf (aref pngarray x y 0) color)
  (setf (aref pngarray x y 1) color)
  (setf (aref pngarray x y 2) color))

(defun matrix->png (matrix fpng version pixsize margin)
  (let* ((modules (matrix-modules version))
	 (size (+ (* modules pixsize) (* margin 2)))
	 (qrpng (make-instance 'zpng:png :width size :height size))
	 (qrarray (zpng:data-array qrpng)))
    (do ((x 0 (1+ x)))
	((>= x size))
      (do ((y 0 (1+ y)))
	  ((>= y size))
	(if (and (<= margin x (- size margin 1))
		 (<= margin y (- size margin 1)))
	    (let ((i (floor (- x margin) pixsize))
		  (j (floor (- y margin) pixsize)))
	      (if (dark-module-p matrix i j)
		  (set-color qrarray x y 0)
		  (set-color qrarray x y 255)))
	    (set-color qrarray x y 255))))
    (zpng:write-png qrpng fpng :if-exists :supersede)))