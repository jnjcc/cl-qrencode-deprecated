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

;;;; qr-mask.lisp
;;;; This file contains data mask

(in-package #:cl-qrencode)

(defun copy-matrix (matrix)
  (let* ((width (floor (sqrt (array-total-size matrix))))
	 (ret (make-array `(,width ,width))))
    (loop for i from 0 to (- width 1) do
	 (loop for j from 0 to (- width 1) do
	      (setf (aref ret i j) (aref matrix i j))))
    ret))

;; masking applied to encoding region (excluding FI && VI)
(defun mask-matrix-single (matrix indicator)
  "Mask matrix using INDICATOR-th mask pattern"
  (let ((ret (copy-matrix matrix))
	(width (floor (sqrt (array-total-size matrix))))
	(mask-p (mask-condition indicator)))
    (loop for i from 0 to (- width 1) do
	 (loop for j from 0 to (- width 1) do
	      (when (and (or (= (aref ret i j) 0)
			     (= (aref ret i j) 1))
			 (funcall mask-p i j))
		(setf (aref ret i j) (mod (+ (aref ret i j) 1) 2)))))
    ret))

(defun mask-matrix (matrix)
  "Returns masked matrix & mask pattern reference"
  ; 8 mask patterns
  (let ((result (make-array 8))
	(penalty 0)
	(pattern 0))
    (loop for i from 0 to 7 do
	 (setf (aref result i) (mask-matrix-single matrix i)))

    (loop for i from 0 to 7 do
	 (let ((mpenalty (mask-penalty (aref result i))))
	   (when (or (= i 0)
		     (> penalty mpenalty))
	     (setf penalty mpenalty)
	     (setf pattern i))))
    (values (aref result pattern) pattern)))

(defun mask-penalty (matrix)
  (let ((width (floor (sqrt (array-total-size matrix))))
	(penalty 0))

    ; feature 1: N1 = 3
    (loop for row from 0 to (- width 1) do
	 (loop for col from 0 to (- width 1) do
	      (let ((nr-same 0)
		    (color (aref matrix row col)))
		(loop for r from -1 to 1 do
		     (unless (or (< (+ row r) 0) (<= width (+ row r)))
		       (loop for c from -1 to 1 do
			    (unless (or (< (+ col c) 0) (<= width (+ col c))
				      (= r c 0))
			      (when (same-color-p color (aref matrix (+ row r) (+ col c)))
				(incf nr-same))))))
		(when (> nr-same 5)
		  (incf penalty (- nr-same 5))))))

    ; feature 2: N2 = 3
    (loop for row from 0 to (- width 2) do
	 (loop for col from 0 to (- width 2) do
	      (let ((bcount 0))
		(when (dark-module-p matrix row col)
		  (incf bcount))
		(when (dark-module-p matrix (+ row 1) col)
		  (incf bcount))
		(when (dark-module-p matrix row (+ col 1))
		  (incf bcount))
		(when (dark-module-p matrix (+ row 1) (+ col 1))
		  (incf bcount))
		(when (or (= bcount 0) (= bcount 4))
		  (incf penalty 3)))))

    ; feature 3: N3 = 40
    (loop for row from 0 to (- width 1) do
	 (loop for col from 0 to (- width 7) do
	      (when (and (dark-module-p matrix row col)
			 (not (dark-module-p matrix row (+ col 1)))
			 (dark-module-p matrix row (+ col 2))
			 (dark-module-p matrix row (+ col 3))
			 (dark-module-p matrix row (+ col 4))
			 (not (dark-module-p matrix row (+ col 5)))
			 (dark-module-p matrix row (+ col 6)))
		(incf penalty 40))))
    (loop for row from 0 to (- width 7) do
	 (loop for col from 0 to (- width 1) do
	      (when (and (dark-module-p matrix row col)
			 (not (dark-module-p matrix (+ row 1) col))
			 (dark-module-p matrix (+ row 2) col)
			 (dark-module-p matrix (+ row 3) col)
			 (dark-module-p matrix (+ row 4) col)
			 (not (dark-module-p matrix (+ row 5) col))
			 (dark-module-p matrix (+ row 6) col))
		(incf penalty 40))))
    
    ; feature 4: N4 = 10; TODO: too much redundancy!!
    (let ((darkcount 0)
	  (ratio 0))
      (loop for row from 0 to (- width 1) do
	   (loop for col from 0 to (- width 1) do
		(when (dark-module-p matrix row col)
		  (incf darkcount))))
      (setf ratio (/ (abs (- (/ (* 100 darkcount) width width) 50)) 5))
      (incf penalty (* ratio 10)))

    penalty))
