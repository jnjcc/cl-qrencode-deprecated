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
;;;; This file contains The Lord of the Rings
;;;;

(in-package #:cl-qrencode)

;;------------------------------------------------------------------------------
;; 1. Convert input into bstring: we've done that in qr-bstring.lisp
;; 2. Data allocation based on the bstring.
;;------------------------------------------------------------------------------
;; A dark module is 1/4, a light module is 0/3.
;; NOTICE: do not change these definitions
(defvar *blank* 2 "for encoding region")
;; 1/0 are resvered for masking
(defvar *fdark* 4 "function pattern dark module")
(defvar *flight* 3 "function pattern light module")
(defun make-matrix (version &key (blank *blank*))
  "Make an empty matrix based on the version of current input,
we use BLANK to indicate the emptyness."
  (declare (type number version))
  (let ((modules (matrix-modules version)))
    (make-array `(,modules ,modules) :initial-element blank)))
(defun dark-module-p (matrix i j)
  (or (= (aref matrix i j) 1)
      (= (aref matrix i j) 4)))
(defun same-color-p (color1 color2)
  (case color1
    ((0 1) (or (= color1 color2) (= (+ color1 3) color2)))
    ((3 4) (or (= color1 color2) (= (- color1 3) color2)))
    (otherwise (= color1 color2))))
;;------------------------------------------------------------------------------
;; The QRCode symbol is consisted of two parts: Function Patterns & Encoding region.
;;   Here is the first main part: Function Patterns
;;------------------------------------------------------------------------------
;; (1) Position Detection Patterns
(defun paint-square (matrix x y n color)
  "Paint a square of size N from upleft (X, Y) in MATRIX to COLOR"
  (let ((maxx (+ x (- n 1)))
	(maxy (+ y (- n 1))))
    (loop for i from x to maxx do
	 (loop for j from y to maxy do
	      (setf (aref matrix i j) color)))
    matrix))
(defun paint-one-position (matrix x y)
  "Paint one Positioin Detection Pattern with upper left at (X, Y)."
  (setf matrix (paint-square matrix x y 7 *fdark*))
  (setf matrix (paint-square matrix (+ x 1) (+ y 1) 5 *flight*))
  (paint-square matrix (+ x 2) (+ y 2) 3 *fdark*))
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
	 (setf (aref matrix 7 j) *flight*)
	 (setf (aref matrix 7 (- modules j 1)) *flight*)
	 (setf (aref matrix (- modules 8) j) *flight*))
    (loop for i from 0 to 7 do ; Vertical separators.
	 (setf (aref matrix i 7) *flight*)
	 (setf (aref matrix (- modules i 1) 7) *flight*)
	 (setf (aref matrix i (- modules 8)) *flight*))
    matrix))
;; (3) Timing Patterns.
(defun paint-timing (matrix version) ;modules)
  (let ((modules (matrix-modules version)))
    (loop for idx from 8 to (- modules 9) do
	 (if (evenp idx)
	     (progn 
	       (setf (aref matrix 6 idx) *fdark*) ; Horizontal
	       (setf (aref matrix idx 6) *fdark*)); Vertical
	     (progn
	       (setf (aref matrix 6 idx) *flight*)
	       (setf (aref matrix idx 6) *flight*))))
    matrix))
;; NOTICE: to be precise, FI and VI belongs to Encoding Region
(defun reserve-information (matrix version &key (blank *blank*)) ; instead of MODULES
  "Reserve modules for Format information and Version information, by 'reserve', we
mean set the modules to *FLIGHT*."
  ; First, reserve Format information modules
  (let ((modules (matrix-modules version)))
    (loop for j from 0 to 7 do ; Horizontal
	 (when (= (aref matrix 8 j) blank)
	   (setf (aref matrix 8 j) *flight*))
	 (setf (aref matrix 8 (- modules j 1)) *flight*))
    (setf (aref matrix 8 8) *flight*)
    (loop for i from 0 to 7 do ; Vertical
	 (when (= (aref matrix i 8) blank)
	   (setf (aref matrix i 8) *flight*))
	 (setf (aref matrix (- modules i 1) 8) *flight*))
    (setf (aref matrix (- modules 8) 8) *fdark*)

    ; Reserve Version information modules for Version 7 ~ 40
    (when (>= version 7)
      (loop for j from 0 to 5 do
	   (loop for i from (- modules 11) to (- modules 9) do
		(setf (aref matrix i j) *flight*)
		(setf (aref matrix j i) *flight*))))
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
;; NOTICE: Alignment Patterns, only in version 2 or higher
(defun paint-one-align (matrix x y)
  "Note: here, (X, Y) is the center of the square."
  (setf matrix (paint-square matrix (- x 2) (- y 2) 5 *fdark*))
  (setf matrix (paint-square matrix (- x 1) (- y 1) 3 *flight*))
  (paint-square matrix x y 1 *fdark*))
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
(defun blank-p (matrix i j &optional (blank *blank*))
  (let ((width (floor (sqrt (array-total-size matrix)))))
    (and (<= 0 i (- width 1))
	 (<= 0 j (- width 1))
	 (= (aref matrix i j) blank))))
(defun paint-encoding (bstring matrix version &key (blank *blank*))
  "The second main part: Paint the Encoding Region."
  (let* ((modules (matrix-modules version))
	 (i (- modules 1))
	 (j (- modules 1))
	 (direction -1) ; -1 means upwards, 1 means downwards
	 (len (length bstring)))
    (do ((idx 0))
	((= idx len))
      (when (blank-p matrix i j blank)
	;(format t "~%(~A, ~A) ~A~%" i j idx)
	(setf (aref matrix i j) (- (char-code (aref bstring idx))
				   (char-code #\0)))
	(incf idx))
      (when (and (>= (- j 1) 0)
		 (blank-p matrix i (- j 1) blank))
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

(defun infochar->color (char)
  "Helper function, translate information bstring character into color:
*flight* / *fdark*"
  (+ 3 (- (char-code char)
	  (char-code #\0))))
;; After masking, paint format information
(defun paint-format (fbstring matrix modules)
  (assert (= (length fbstring) 15))
  (let ((idx 0)
	(idx2 0))
    ; horizontal 14 ~ 8
    (loop for j from 0 to 7 do
	 (when (= (aref matrix 8 j) *flight*)
	   (setf (aref matrix 8 j) (infochar->color (aref fbstring idx)))
	   (incf idx)))
    ; vertical 14 ~ 8
    (loop for i from (- modules 1) downto (- modules 7) do
	 (setf (aref matrix i 8) (infochar->color (aref fbstring idx2)))
	 (incf idx2))
    ; horizontal 7 ~ 0
    (loop for j from (- modules 8) downto (- modules 1) do
	 (setf (aref matrix 8 j) (infochar->color (aref fbstring idx)))
	 (incf idx))
    ; vertical 7 ~ 0
    (loop for i from 8 downto 0 do
	 (when (= (aref matrix i 8) *flight*)
	   (setf (aref matrix i 8) (infochar->color (aref fbstring idx2)))
	   (incf idx2))))
  matrix)
;; Paint Version information
(defun paint-version (vbstring matrix modules)
  (assert (= (length vbstring) 18))
  (let ((j 5)
	(i (- modules 9))
	(start (- modules 9))
	(border (- modules 11)))
    (loop for idx from 0 to 17 do
	 (setf (aref matrix i j) (infochar->color (aref vbstring idx)))
	 (setf (aref matrix j i) (infochar->color (aref vbstring idx)))
	 (if (>= (- i 1) border)
	     (decf i)
	     (progn
	       (decf j)
	       (setf i start))))))

(defun codeword-placement (bstring version correct &key (blank *blank*))
  (let* ((matrix (make-matrix version :blank blank)))
    (setf matrix (paint-function-pattern matrix version :blank blank))
    ;(print matrix)
    (setf matrix (paint-encoding bstring matrix version :blank blank))
    
    ; mask pattern
    (multiple-value-bind (matrix pat)
	(mask-matrix matrix)
      ; format information
      (let ((fbstring (concatenate 'string (aref *errc-indicator* (corrlev->index correct))
				   (aref *mask-pattern-ref* pat))))
	(paint-format (info->bstring fbstring) matrix (matrix-modules version)))
      ; Version information
      (when (>= version 7)
	(let ((vbstring (decimal->bstring version 6)))
	  (paint-version (info->bstring vbstring :format-p nil) matrix (matrix-modules version))))
      
      (dbg :qr-matrix "~A~%" matrix)
      matrix)))
    
(defun reserved-module-p (x y version)
  "If (X, Y) is reserved by Function Pattern, or Format Information, or Version
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
(defun qrencode (text &key (fpng "out.png") (version 1) (mode :binary)
		 (correction :level-q) (pixsize 9) (margin 6))
  (declare (type string text) (type number version)
	   (type symbol mode correction))
  ; (sdebug :qr-input :qr-errc :qr-matrix)
  (sdebug :qr-input-core)
  (let* ((input (string->input text :version version
			       :mode mode :correction correction))
	 (bstring (input->bstring input)))
    (dbg :qr-input "~A" bstring)
    (dbg :qr-input "~A" (length bstring))
    ; version adusted
    (with-slots (version) input
      (let ((matrix (codeword-placement bstring version correction)))
	(matrix->png matrix fpng version pixsize margin))))
  (format t "png file `~A' wrote..." fpng)
  (values))
