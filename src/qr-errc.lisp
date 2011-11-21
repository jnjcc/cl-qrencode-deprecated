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

;;;; qr-errc.lisp
;;;; This file contains utilities that generate error correction polynomial
;;;; using Reed-Solomon Error Correction, or BCH code.
;;;;

(in-package #:cl-qrencode)

;;------------------------------------------------------------------------------
;; This is the implementation of Polynomial using association list
;; ((cofficient . exponent) ...), where each element is called a term.
;; There are two kinds of polynomial as to our Reed-Solomon error correction:
;; 1. Message Polynomial: the cofficients are the codewords
;; 2. Generator Polynomial: the cofficients are the exponent of generator A
;;------------------------------------------------------------------------------
(defclass qr-polynomial ()
  ((polynomial :initarg :polynomial
	       :accessor polynomial
	       :initform nil)))
(defclass message-polynomial (qr-polynomial)
  ())
(defclass generator-polynomial (qr-polynomial)
  ())
(defun print-term (stream coff exp &key (message-p t) (last-p nil))
  "Print one term of the polynomial. MESSAGE-POLYNOMIAL as default."
  (let ((mul-sign nil)) ;; is there "*" or not
    (if message-p
	(cond
	  ((and (= coff 1) (= exp 0)) (format stream "1"))
	  ((not (= coff 1)) (progn
			      (format stream "~A" coff)
			      (setf mul-sign t))))
	(cond
	  ((and (= coff 0) (= exp 0)) (format stream "1"))
	  ((= coff 1) (progn (format stream "a")
			     (setf mul-sign t)))
	  ((not (= coff 0)) (progn (format stream "a^~A" coff)
				   (setf mul-sign t)))))
    (cond ((= exp 1) (progn (when mul-sign
			      (format stream "*"))
			    (format stream "x")))
	  ((not (= exp 0)) (progn (when mul-sign
				    (format stream "*"))
				  (format stream "x^~A" exp)))))
  (when (not last-p)
    (format stream " + ")))
(defmethod print-object ((msgobj message-polynomial) stream)
  (fresh-line)
  (format stream ">>>>>>>>>>>>>>>>>>>>>>>>>>>>~%")
  (format stream "Object of class MESSAGE-POLYNOMIAL:~%~T")
  (let* ((poly (polynomial msgobj))
	 (size (- (length poly) 1)))
    (dotimes (i (+ size 1))
      (let ((term (nth i poly)))
	(print-term stream (car term) (cdr term) :message-p t
		    :last-p (= i size)))))
  (format stream "~%<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%"))
(defmethod print-object ((genobj generator-polynomial) stream)
  (fresh-line)
  (format stream ">>>>>>>>>>>>>>>>>>>>>>>>>>>>~%")
  (format stream "Object of class GENERATOR-POLYNOMIAL:~%~T")
  (let* ((poly (polynomial genobj))
	 (size (- (length poly) 1)))
    (dotimes (i (+ size 1))
      (let ((term (nth i poly)))
	(print-term stream (car term) (cdr term) :message-p nil
		    :last-p (= i size)))))
  (format stream "~%<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%"))

;;------------------------------------------------------------------------------
;; This is (part of) the implementation of Galois Field GF(2^8)
;; 1. Assume A is the primitive element of GF(2^8), so A^255 = 1, we will consider as
;;     A^256 = A^1, A^257 = A^2, etc.
;; 2. *GF* is order of GF(2^8)
;; 3. *LOG* represents the corresponding exponent of A for each element in GF(2^8)
;; 4. *AEXP* represents the corresponding result for each A^elem
;; 5. *PP* Prime Polynomial in use is x^8 + x^4 + x^3 + x^2 + 1, or 1,0001,1101, or 285
;;------------------------------------------------------------------------------
(defvar *GF* 256)
(defvar *PP* 285)
; Let's assume (log 0) == -1
(defvar *LOG* (make-array *GF* :initial-element -1))
; Let's set A^0 = 1
(defvar *AEXP* (make-array *GF* :initial-element 1))

; This function must be called first before using *LOG* and *AEXP*
(defun init-galois-field ()
  "Generate the cyclic group from the generator A, or we can say, this is the notation
of multiplication for this GF(2^8)."
  (loop for i from 1 to (- *GF* 1) do
       (setf (aref *AEXP* i) (* (aref *AEXP* (- i 1))
				2))
       (when (>= (aref *AEXP* i) *GF*)
	 (setf (aref *AEXP* i) (boole boole-xor (aref *AEXP* i) *PP*)))
       (setf (aref *LOG* (aref *AEXP* i)) i))
  (setf (aref *LOG* 1) 0))

(defun sort-polynomial (polynomial &key (message-p t))
  "Sort polynomial according to exponent of the X-part, if it is a message polynomial,
we shall delete those terms whose cofficient are 0."
  (when message-p
    (setf polynomial (remove-if #'zerop polynomial :key #'car)))
  (sort polynomial #'> :key #'cdr))
  
; Addition Group for this Galois Field GF(2^8)
(defmethod addition ((msgobj message-polynomial) (genobj generator-polynomial))
  "Addition for GF(2^8) between a message polynomial and a generator polynomial,
exclusive-or operation to ensure closure, in fact."
  (let ((msgpoly (copy-tree (polynomial msgobj)))
	(genpoly (copy-tree (polynomial genobj)))
	(result (make-instance 'message-polynomial)))
    (loop for term in genpoly do
	 (let ((xexp (cdr term)))
	   (if (rassoc xexp msgpoly)
	       (setf (car (rassoc xexp msgpoly))
		     (boole boole-xor (car (rassoc xexp msgpoly))
			    (aref *AEXP* (car term))))
	       (setf msgpoly (append msgpoly (list (cons (aref *AEXP* (car term))
							 (cdr term))))))))
    (setf (polynomial result) (sort-polynomial msgpoly))
    result))
(defmethod addition ((genobj generator-polynomial) (msgobj message-polynomial))
  "This is a group, so the addition operation should be commutative."
  (addition msgobj genobj))
(defmethod addition ((genobj1 generator-polynomial) (genobj2 generator-polynomial))
  "Addition fo GF(2^8) between two generator polynomial: obtain corresponding integer
of A^exp, XOR them, and then obtain the corresponding exponent."
  (let ((genpoly1 (polynomial genobj1))
	(genpoly2 (polynomial genobj2))
	(result (make-instance 'generator-polynomial)))
    (loop for term in genpoly2 do
	 (let ((xexp (cdr term)))
	   (if (rassoc xexp genpoly1)
	       (setf (car (rassoc xexp genpoly1))
		     (aref *LOG* (boole boole-xor (aref *AEXP* (car (rassoc xexp genpoly1)))
					(aref *AEXP* (car term)))))
	       (setf genpoly1 (append genpoly1 (list term))))))
    (setf (polynomial result) (sort-polynomial genpoly1 :message-p nil))
    result))

; Multiplication Group for Galois Field GF(256)
; delta value for exp of generator A, delta value for exp of x
(defun multiply-each-term (genpoly &key (adelta 0) (xdelta 1) (part :cdr))
  "Helper function for multiply generator polynomial.
Once some exponent of A surpasses *GF*, we do the math [(exp % *GF*) + 1]."
  (labels ((modulo (exp-sum)
	     (if (>= exp-sum *GF*)
		 (+ (mod exp-sum *GF*) 1)
		 exp-sum))
	   (adder (term)
	     (case part
	       (:car (cons (modulo (+ (car term) adelta))
			   (cdr term)))
	       (:cdr (cons (car term)
			   (modulo (+ (cdr term) xdelta))))
	       (:both (cons (modulo (+ (car term) adelta))
			    (modulo (+ (cdr term) xdelta)))))))
    (mapcar #'adder genpoly)))
(defmethod multiplication ((genobj1 generator-polynomial) (genobj2 generator-polynomial))
  "Multiplication for GF(2^8) between two generator polynomial.
We only consider genobj2 as ((0 . 1) (t . 0)), where t's max value is # of error
correction word _MINUS_ 1."
  (let ((genpoly1 (polynomial genobj1))
	(genpoly2 (polynomial genobj2))
	(tmpobj1 (make-instance 'generator-polynomial))
	(tmpobj2 (make-instance 'generator-polynomial))
	(result (make-instance 'generator-polynomial)))
    (setf (polynomial tmpobj1) (multiply-each-term genpoly1
						   :xdelta 1
						   :part :cdr))
    (setf (polynomial tmpobj2) (multiply-each-term genpoly1
						   :adelta (car (second genpoly2))
						   :part :car))
    (setf result (addition tmpobj1 tmpobj2))
    (setf (polynomial result) (sort-polynomial (polynomial result) :message-p nil))
    ; (print (polynomial result))
    result))

; Generate message polynomial from bstring
(defun bstring->decimal (bstring)
  "Helper function, convert a sequence of 0-1 bstring to number."
  (declare (type string bstring))
  (let* ((bstring (reverse bstring))
	 (len (length bstring))
	 (result 0))
    (dotimes (i len)
      (when (char= (char bstring i) #\1)
	(incf result (ash 1 i))))
    result))
(defun generate-msgpoly (bstring version errc)
  "Convert bstring to message polynomial, length should be times of 8."
  (declare (type string bstring)
	   (type number version)
	   (type symbol errc))
  (let ((exponents (- (+ (nr-data-codewords version errc) 
			 (nr-errc-codewords version errc))
		     1))
	(terms (/ (length bstring) 8))
	(result (make-instance 'message-polynomial)))
    (dotimes (i terms)
      (let ((beg (* i 8)))
	(setf (polynomial result) (append (polynomial result)
					  (list (cons (bstring->decimal (subseq bstring beg (+ beg 8)))
						      (- exponents i)))))))
    (setf (polynomial result) (sort-polynomial (polynomial result) :message-p t))
    result))
; Generate generator polynomial according to number of error correction words.
(defun generate-genobj (nr-errc-words)
  "(x - A^0)(x - A^1)...(x - A^(n-1)), where A is the generator, and n
is the number of error correction words."
  (declare (type number nr-errc-words))
  ; We are trying to use the generator group, so init them first
  (init-galois-field)
  (let ((result (make-instance 'generator-polynomial))
	(multiplier (make-instance 'generator-polynomial)))
    (setf (polynomial result) '((0 . 1) (0 . 0)))
    (loop for i from 1 to (- nr-errc-words 1) do
	 (setf (polynomial multiplier) `((0 . 1) (,i . 0)))
	 (setf result (multiplication result multiplier)))
    result))

(defun generate-errcobj (bstring version errc)
  "Generate error correction polynomial. This in fact is the division operation of GF(2^8):
1. Multiply the generator polynomial so that the result's first term equals message polynomial.
2. XOR the multiply result so as to erase the first term of message polynomial.
3. Repeat 1 & 2 until the first term of result is less than generator polynomial."
  (declare (type string bstring)
	   (type number version)
	   (type symbol errc))
  (let* ((msgobj (generate-msgpoly bstring version errc))
	 (genobj (generate-genobj (nr-errc-codewords version errc)))
	 (genpoly (polynomial genobj))
	 (tmpobj (make-instance 'generator-polynomial)))
    (princ "Message Polynomial:")
    (print msgobj)
    (princ "Generator Polynomial:")
    (print genobj)
    (do ()
	((< (cdr (first (polynomial msgobj)))
	    (cdr (first genpoly))))
      (let ((adelta (aref *LOG* (car (first (polynomial msgobj)))))
	    (xdelta (- (cdr (first (polynomial msgobj)))
		       (cdr (first genpoly)))))
	(setf (polynomial tmpobj) (multiply-each-term genpoly :adelta adelta
						      :xdelta xdelta :part :both))
	(setf msgobj (addition msgobj tmpobj))))
    (princ "Errc Polynomial:")
    (print msgobj)
    msgobj))

; After all, we have to change the error correction polynomial back to bstring
(defun errcobj->bstring (errcobj)
  "Walk through the polynomial, translate each cofficient back into bstring
using 8 bits."
  (let ((errcpoly (polynomial errcobj))
	(bstring ""))
    (loop for term in errcpoly do
	 (setf bstring (concatenate 'string bstring
				    (decimal->bstring (car term) 8))))
    bstring))

;;------------------------------------------------------------------------------
;; This is the Error Correction for Format Information and Version Information
;; 1. Bose-Chaudhuri-Hocquenghem(BCH) (15, 5) code for former, generator polynomial is
;;      x^10 + x^8 + x^5 + x^4 + x^2 + x + 1
;; 2. Bose-Chaudhuri-Hocquenghem(BCH) (18, 6) code for latter, generator polynomial is
;;      x^12 + x^11 + x^10 + x^9 + x^8 + x^5 + x^2
;; NOTE: This time, it is the exponent that matters, not the cofficient.
;;------------------------------------------------------------------------------
(defun info->msgobj (bstring &key (format-p t))
  "Translate Format Information or Version Information BSTRING to a Message Polynomial.
Format Information is consisted of 5 bits: 2 *ERRC-INDICATOR*, 3 *MASK-PATTERN-REF*.
Version Information is consisted of 6 bits, all used to encode Version.
FORMAT-P means bstring is Format Information, otherwise, Version Information."
  (declare (type string bstring))
  ; (print format-p)
  (when (and format-p (not (= (length bstring) 5)))
    (error 'qr-bad-arguments :file-name "qr-errc.lisp"
	   :function-name "info->msgpoly" :arguments bstring
	   :how "Format Information is consisted of 5 bits."))
  (when (and (not format-p) (not (= (length bstring) 6)))
    (error 'qr-bad-arguments :file-name "qr-errc.lisp"
	   :function-name "info->msgpoly" :arguments bstring
	   :how "Version Information is consisted of 6 bits."))
  (let ((msgobj (make-instance 'message-polynomial))
	(len (length bstring))
	(xdelta 12)
	poly)
    (when format-p
      (setf xdelta 10))
    (dotimes (i len)
      (when (char= (char bstring i) #\1)
	(setf poly (append poly (list (cons 1 (- len 1 i)))))))
    (setf (polynomial msgobj) (multiply-each-term poly :xdelta xdelta))
    msgobj))
(defun info->errcpoly (bstring &key (format-p t))
  "Do error correction for BSTRING. BCH(15, 5) to Format Information BSTRING, or
BCH(18, 6) for Version. Different from above, that coffients are all 1 for Message
Polynomial(0 for Generator Polynomial, of course)."
  (declare (type string bstring))
  (let ((msgobj (info->msgobj bstring :format-p format-p))
	(genobj (make-instance 'generator-polynomial))
	(tmpobj (make-instance 'generator-polynomial)))
    (princ "Format/Version Information Polynomial:")
    (print msgobj)
    (if format-p
	(setf (polynomial genobj) *format-generator*)
	(setf (polynomial genobj) *version-generator*))
    (princ "Format/Version Information Generator Polynomial:")
    (print genobj)
    (do ((genpoly (polynomial genobj)))
	((< (cdr (first (polynomial msgobj)))
	    (cdr (first genpoly))))
      (let ((xdelta (- (cdr (first (polynomial msgobj)))
		       (cdr (first genpoly)))))
	(setf (polynomial tmpobj) (multiply-each-term genpoly :xdelta xdelta))
	(setf msgobj (addition msgobj tmpobj))))
    (princ "Format/Version Information Errc Polynomial:")
    (print msgobj)
    msgobj))
(defun generate-info-errc (bstring &key (format-p t))
  "Generate Error Correction bits for Format/Version Information"
  (let* ((errcobj (info->errcpoly bstring :format-p format-p))
	 (nrerrc 12) ; # of Version Information Errc bits
	 (errcpoly (polynomial errcobj))
	 errcbits)
    (when format-p
      (setf nrerrc 10))
    (setf errcbits (make-string nrerrc :initial-element #\0))
    (labels ((set-bit (term)
	       (setf (char errcbits (- nrerrc 1 (cdr term))) #\1)))
      (mapcar #'set-bit errcpoly))
    errcbits))
(defun info->bstring (bstring &key (format-p t))
  "Do Error Correction for the Format/Version Information, concatenate them, and,
for Format Information, XOR it with the mask 101010000010010."
  (let ((errcbits (generate-info-errc bstring :format-p format-p))
	(mask "101010000010010"))
    (setf bstring (concatenate 'string bstring errcbits))
    (when format-p
      (setf bstring (map 'string #'(lambda (char1 char2)
				     (if (char= char1 char2) #\0 #\1))
			 bstring mask)))
    (print (length bstring))
    bstring))
