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

;;;; qr-encode-testcase.lisp
;;;; This file contains some testcases for the encoder.
;;;;

(in-package #:cl-qrencode)

;; Version 1.3.2?! That was a joke...

(defvar *data* "00100000010000011100110101000101001010011101110000101\
110100000001110110000101010100111110100101011011101111101001010100111\
101111100101101000101001000110111011010101010111100000011000000100101\
01101101100111101"
  "This is the result when encoding ABCDE123 using :alphanumeric mode and 
:level-h error correction level.")
(setf *data* (remove #\Newline *data*))

;; FIXME: All right, from now on, I just consider Version 1
;; TODO: Further development needed as for Version 2 ~ 40.
;; We use 1 as black modules, 0 as white modules.
(defun char2int (pattern i)
  (if (char= (char pattern i) #\0)
      0
      1))
(defun encode-finder (matrix)
  (let ((pattern1 "1111111")
	(pattern2 "1000001")
	(pattern3 "1011101"))
    (loop for i from 0 to 6 do
         ; 1. all pattern1
	 (setf (aref matrix i 0) (char2int pattern1 i))
	 (setf (aref matrix (+ i 14) 0) (char2int pattern1 i))
	 
	 (setf (aref matrix i 6) (char2int pattern1 i))
	 (setf (aref matrix (+ i 14) 6) (char2int pattern1 i))
	 
	 (setf (aref matrix i 14) (char2int pattern1 i))
	 
	 (setf (aref matrix i 20) (char2int pattern1 i))
	 
         ; 2. all pattern2
	 (setf (aref matrix i 1) (char2int pattern2 i))
	 (setf (aref matrix (+ i 14) 1) (char2int pattern2 i))
	 
	 (setf (aref matrix i 5) (char2int pattern2 i))
	 (setf (aref matrix (+ i 14) 5) (char2int pattern2 i))
	 
	 (setf (aref matrix i 15) (char2int pattern2 i))
	 
	 (setf (aref matrix i 19) (char2int pattern2 i))
	 
         ; 3. all pattern3
	 (loop for j from 2 to 4 do
	      (setf (aref matrix i j) (char2int pattern3 i))
	      (setf (aref matrix (+ i 14) j) (char2int pattern3 i))
	      (setf (aref matrix i (+ j 14)) (char2int pattern3 i))))
    (loop for i from 0 to 7 do
	 (setf (aref matrix i 7) 0)
	 (setf (aref matrix (+ i 13) 7) 0)
	 (setf (aref matrix i 13) 0))
    (loop for j from 0 to 7 do
	 (setf (aref matrix 7 j) 0)
	 (setf (aref matrix 13 j) 0)
	 (setf (aref matrix 7 (+ j 13)) 0)))
  matrix)
(defun encode-timing (matrix)
  (let ((pattern "10101"))
    (loop for i from 8 to 12 do
	 (setf (aref matrix i 6) (char2int pattern (- i 8)))
	 (setf (aref matrix 6 i) (char2int pattern (- i 8))))
    matrix))
(defun reserve-format (matrix)
  (loop for i from 0 to 8 do
       (when (= (aref matrix i 8) *blank*)
	 (setf (aref matrix i 8) 0))
       (when (= (aref matrix 8 i) *blank*)
	 (setf (aref matrix 8 i) 0)))
  (loop for i from 13 to 20 do
       (setf (aref matrix i 8) 0))
  (loop for j from 14 to 20 do
       (setf (aref matrix 8 j) 0))
  (setf (aref matrix 8 13) 1)
  matrix)
(defun encode-fix-part (matrix)
  "Here, we encode the Finder Pattern and Timing Pattern. Also, we reserve places
for Error Correction Level and Mask Pattern (Format Information, we may say)."
  (reserve-format (encode-timing (encode-finder matrix))))

(defun encode-bstring (matrix bstring)
  (let ((i 20) (j 20) (cnt 0)
	(direction -1)
	(length (length bstring)))
    (do ()
	((= cnt length))
      (setf (aref matrix i j) (char2int bstring cnt))
      (incf cnt)
      (when (not (= (aref matrix (- i 1) j) *blank*))
	(setf (aref matrix (- i 1) j) (char2int bstring cnt))
	(incf cnt))
      (if (and (>= (+ j direction) 0)
	       (<= (+ j direction) 20))
	  (incf j direction)
	  (progn
	    (decf i 2)
	    (setf direction (- direction))))))
  matrix)

(defun generate-png (qr-file)
  (let* ((matrix (make-matrix 1))
	 (qr-png (make-instance 'png :width (matrix-modules 1)
				:height (matrix-modules 1)))
	 (png-array (data-array qr-png)))
    (setf matrix (encode-fix-part matrix))
    (setf matrix (encode-bstring matrix *data*))
    (loop for x from 0 to 20 do
	 (loop for y from 0 to 20 do
	      (if (or (= (aref matrix x y) *blank*)
		      (= (aref matrix x y) 0))
		  (progn
		    (setf (aref png-array y x 0) 255)
		    (setf (aref png-array y x 1) 255)
		    (setf (aref png-array y x 2) 255))
		  (progn
		    (setf (aref png-array y x 0) 0)
		    (setf (aref png-array y x 1) 0)
		    (setf (aref png-array y x 2) 0)))))
    (write-png qr-png qr-file :if-exists :supersede)))
