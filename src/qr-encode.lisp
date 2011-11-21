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

;;;; qr-encode.lisp
;;;; This file contains utilities that generate the final png file.
;;;;

(in-package #:cl-qrencode)

;;------------------------------------------------------------------------------
;; 1. Convert input into bstring: we've done that in qr-bstring.lisp
;; 2. Data allocation based on the bstring.
;;------------------------------------------------------------------------------
(defvar *blank* 2)
;; A dark module is a binary one, a light module is a binary zero.
(defun make-matrix (version &key (blank *blank*))
  "Make an empty matrix based on the version of current input,
we use BLANK to indicate the emptyness."
  (declare (type number version))
  (when (or (= blank 0) (= blank 1))
    (error 'qr-bad-arguments :file-name "qr-encode.lisp"
	   :function-name "make-matrix" :arguments blank
	   :how "0 or 1 have specific meanings, cannot be used as blank."))
  (let ((modules (matrix-modules version)))
    (make-array `(,modules ,modules) :initial-element blank)))
;;------------------------------------------------------------------------------
;; The QRCode symbol is consisted of two parts: Function Patterns & Encoding region.
;;   Here is the first main part: Function Patterns
;;------------------------------------------------------------------------------
;; (1) Position Detection Patterns
(defun paint-square (matrix x y n color)
  "Paint a square of size N from upleft (X, Y) in MATRIX to COLOR, which is 0 or 1."
  (let ((maxx (+ x (- n 1)))
	(maxy (+ y (- n 1))))
    (loop for i from x to maxx do
	 (loop for j from y to maxy do
	      (setf (aref matrix i j) color)))
    matrix))
(defun paint-one-position (matrix x y)
  "Paint one Positioin Detection Pattern with upper left at (X, Y)."
  (setf matrix (paint-square matrix x y 7 1))
  (setf matrix (paint-square matrix (+ x 1) (+ y 1) 5 0))
  (paint-square matrix (+ x 2) (+ y 2) 3 1))
(defun paint-position (matrix version) ; modules)
  "Paint all three Position Detection Patterns. The reason why we use VERSION instead of
MODULES as an argument is that we want provide a common interface to users."
  (let ((modules (matrix-modules version)))
    (setf matrix (paint-one-position matrix 0 0))
    (setf matrix (paint-one-position matrix 0 (- modules 7)))
    (paint-one-position matrix (- modules 7) 0)))
;; (2) Separators for Position Detection Pattern.
(defun paint-separator (matrix version) ; modules)
  "Paint Separator for Position Detection Pattern. VERSION-MODULES reason as above."
  (let ((modules (matrix-modules version)))
    (loop for j from 0 to 7 do ; Horizontal separators.
	 (setf (aref matrix 7 j) 0)
	 (setf (aref matrix 7 (- modules j 1)) 0)
	 (setf (aref matrix (- modules 8) j) 0))
    (loop for i from 0 to 7 do ; Vertical separators.
	 (setf (aref matrix i 7) 0)
	 (setf (aref matrix (- modules i 1) 7) 0)
	 (setf (aref matrix i (- modules 8)) 0))
    matrix))
;; (3) Timing Patterns.
(defun paint-timing (matrix version) ;modules)
  (let ((modules (matrix-modules version)))
    (loop for idx from 8 to (- modules 9) do
	 (if (evenp idx)
	     (progn 
	       (setf (aref matrix 6 idx) 1) ; Horizontal
	       (setf (aref matrix idx 6) 1)); Vertical
	     (progn
	       (setf (aref matrix 6 idx) 0)
	       (setf (aref matrix idx 6) 0))))
    matrix))
(defun reserve-information (matrix version &key (blank *blank*)) ; instead of MODULES
  "Reserve modules for Format information and Version information, by 'reserve', we
mean set the modules to 0."
  ; First, reserve Format information modules
  (let ((modules (matrix-modules version)))
    (loop for j from 0 to 7 do ; Horizontal
	 (when (= (aref matrix 8 j) blank)
	   (setf (aref matrix 8 j) 0))
	 (setf (aref matrix 8 (- modules j 1)) 0))
    (setf (aref matrix 8 8) 0)
    (loop for i from 0 to 7 do ; Vertical
	 (when (= (aref matrix i 8) blank)
	   (setf (aref matrix i 8) 0))
	 (setf (aref matrix (- modules i 1) 8) 0))
    (setf (aref matrix (- modules 8) 8) 1)
    ; TODO: Second, reserve Verion Information position.
    ; 		Only Version 7 ~ Version 40 need this.
    matrix))
;; Sum up the common Funtion Patterns above.
(defun paint-common-pattern (matrix version &key (blank *blank*))
  "Paint Function Patterns that are common to all Versions:
Position Detection Pattern & Separator & Timing."
  (setf matrix (paint-position matrix version))
  (setf matrix (paint-separator matrix version))
  (setf matrix (paint-timing matrix version))
  (reserve-information matrix version :blank blank))
;; (4) Paint Alignment Patterns. Of course, Alignment is a kind of Function
;;     Pattern, but a little different(depends on version), so we list it separately.
(defun paint-one-align (matrix x y)
  "Note: here, (X, Y) is the center of the square."
  (setf matrix (paint-square matrix (- x 2) (- y 2) 5 1))
  (setf matrix (paint-square matrix (- x 1) (- y 1) 3 0))
  (paint-square matrix x y 1 1))
(defun paint-alignment (matrix version)
  "Paint all the Alignment Patterns under VERSION, Alignment Pattern varies from version
to version."
  (let ((centers (align-centers version)))
    (loop for center in centers do
	 (setf matrix (paint-one-align matrix (first center) (second center))))
    matrix))
(defun paint-function-pattern (matrix version &key (blank *blank*))
  "The first main part: Function Pattern."
  (setf matrix (paint-common-pattern matrix version :blank blank))
  (paint-alignment matrix version))
(defun function-pattern (version &key (blank *blank*))
  "For debug purpose; Matrix status after painting Function Patterns."
  (let ((matrix (make-matrix version :blank blank)))
    (print (paint-function-pattern matrix version :blank blank))
    (values)))
;;------------------------------------------------------------------------------
;; The QRCode symbol is consisted of two parts: Function patterns & Encoding regions.
;;   Here is the second main part: Encoding Region.
;;------------------------------------------------------------------------------
;; Symbol character placement.
(defun paint-encoding (bstring matrix version &key (blank *blank*))
  "The second main part: Paint the Encoding Region."
  (let* ((modules (matrix-modules version))
	 (i (- modules 1))
	 (j (- modules 1))
	 (direction -1) ; -1 means upwards, 1 means downwards
	 (len (length bstring)))
    (do ((idx 0))
	((= idx len))
      (when (= (aref matrix i j) blank)
	;(format t "~%(~A, ~A) ~A~%" i j idx)
	(setf (aref matrix i j) (- (char-code (aref bstring idx))
				   (char-code #\0)))
	(incf idx))
      (when (and (>= (- j 1) 0)
		 (= (aref matrix i (- j 1)) blank))
	;(format t "~%(~A, ~A) ~A~%" i (- j 1) idx)
	(setf (aref matrix i (- j 1)) (- (char-code (aref bstring idx))
					 (char-code #\0)))
	(incf idx))
      (if (< -1 (+ i direction) modules)
	  (incf i direction)
	  (progn
	    (setf direction (- direction))
	    (decf j 2))))
    matrix))
(defun encoding-region (bstring matrix version &key (blank *blank*))
  "For debug purpose; Matrix status after painting Encoding Region."
  (when (= (aref matrix 0 0) blank)
    (error 'qr-bad-arguments :file-name "qr-encode.lisp"
	   :function-name "encoding-region" :arguments 'matrix
	   :how "This function must be called after PAINT-FUNTION-PATTERN."))
  (print (paint-encoding bstring matrix version blank)))

(defun codeword-placement (bstring version &key (blank *blank*))
  (let* ((matrix (make-matrix version :blank blank)))
    (setf matrix (paint-function-pattern matrix version :blank blank))
    ;(print matrix)
    (setf matrix (paint-encoding bstring matrix version :blank blank))
    matrix))

(defun reversed-module-p (x y version)
  "If (X, Y) is reversed by Function Pattern, or Format Information, or Version
Information: they cannot be masked."
  (let ((modules (matrix-modules version)))
    (or (and (<= 0 x 8) (<= 0 y 8))      ; 1) Upper-Left Position Detection Pattern
	(and (<= 0 x 8) (<= (- modules 8) y (- modules 1))) ; 2) Upper-Right PDP
	(and (<= 0 y 8) (<= (- modules 8) x (- modules 1))) ; 3) Lower-Left PDP
	)))
;;------------------------------------------------------------------------------
;; One Ring to Rule Them All, One Ring to Find Them,
;; One Ring to Bring Them All and In the Darkness Blind Them:
;;   This function wraps all we need.
;;------------------------------------------------------------------------------
(defun qrencode (text &key (version 1) (mode :alphanumeric)
		 (correction :level-q))
  (declare (type string text) (type number version)
	   (type symbol mode correction))
  (let* ((input (string->input text :version version
			       :mode mode :correction correction))
	 (bstring (input->bstring input)))
    (print bstring)
    (fresh-line)
    (print (codeword-placement bstring version)))
  (values))
