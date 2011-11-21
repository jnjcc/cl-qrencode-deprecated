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

;;;; qr-bstring.lisp
;;;; This file contains utilities that convert QR-INPUT to binary string (bstring, 
;;;; bitstream, or bit sequence, we may say).
;;;;

(in-package #:cl-qrencode)

;;; 1. Encode *Mode Indicator*.
;;; defined in qr-spec.lisp as (mode->bstring)

;;; 2. Encode *Character Count Indicator*: bit sequence which defines the data string
;;;    length in a mode.
(defun decimal->bstring (num bits)
  "Use BITS bits to translate NUM of decimal into binary."
  (declare (type number num bits))
  (when (< (- (expt 2 bits) 1) num)
    (error 'qr-bad-arguments :file-name "qr-bstring.lisp" :function-name "decmimal->bstring"
	   :arguments '(num bits) :how "Bits not enough to hold num."))
  (let ((bstring ""))
    (dotimes (i bits)
      (if (logbitp i num)
	  (setf bstring (concatenate 'string bstring "1"))
	  (setf bstring (concatenate 'string bstring "0"))))
    (reverse bstring)))

;;; 3. Encode input data string.
(defvar *encode-functions*
  '(numeric->bstring alnum->bstring binary->bstring kanji->bstring)
  "Functions to encode input data string, defined in qr-data-bstring.lisp.")
(defun data->bstring (data mode)
  (declare (type string data)
	   (type symbol mode))
  (let* ((idx (mode->index mode))
	 (encode (nth idx *encode-functions*)))
    (funcall encode data)))

;;; 4. padding-bits
(defun padding-bits (blength)
  "Padding bits into bstring of length BLENGTH so that the length can be times of 8."
  (declare (type number blength))
  (make-string (- 8 (mod blength 8)) :initial-element #\0))
;    (if (<= padding-length 4)
;	; bstring almost fills the capacity of the symbol
;	(make-string padding-length :initial-element #\0)
;	(progn
;	  (incf blength 4)
;	  ; make sure bit string mode 8 is zero
;	  (let ((remainder (- 8 (mod blength 8))))
;	    (when (not (= remainder 8))
;	      (incf blength remainder)
;	      (setf result (concatenate 'string result
;					(make-string remainder :initial-element #\0)))))
;	  (concatenate 'string result
;		       (padding-special-codeword (- bits blength)))))))
(defun padding-pad-codeword (remain-bits)
  "Padding Pad Codewords 11101100 & 00010001 to fill data codeword capacity of symbol. 
We have assured that REMAIN-BITS is multiple of 8 in function (padding-bits)."
  (declare (type number remain-bits))
  (when (not (= (mod remain-bits 8) 0))
    (error 'qr-bad-arguments :file-name "qr-bstring.lisp"
	   :function-name "padding-pad-codeword" :arguments remain-bits
	   :how "BLENGTH shall be times of eight, something went wrong..."))
  (let ((bytes (/ remain-bits 8))
	(result ""))
    (dotimes (idx bytes)
      (if (evenp idx)
	  (setf result (concatenate 'string result
				    "11101100"))
	  (setf result (concatenate 'string result
				    "00010001"))))
    result))
(defun encode-data (input)
  "Encode data into bstring."
  (declare (type qr-input input))
  (let* ((bstring "")
	 (terminator "0000")
	 (version (version input))
	 (mode (mode input)))
    ; 0. TODO: Encode ECI mode indicator
    ; 1. Encode Mode Indicator
    (setf bstring (concatenate 'string bstring
			       (mode->bstring mode)))
    ; 2. Encode Character Count Indicator
    (setf bstring (concatenate 'string bstring
			       (decimal->bstring (length (data input))
						 (count-indicator-bits version mode))))

    ; 3. Encode input data string
    (setf bstring (concatenate 'string bstring
			       (data->bstring (data input) mode)))

    ; 4. Add Terminator "0000"
    (setf bstring (concatenate 'string bstring terminator))
    bstring))
;;; The overall function to encode qr-input to bstring.
(defun input->bstring (input)
  (declare (type qr-input input))
  (let* ((bstring "")
	 (version (version input))
	 (correct (correction input)))
    ; 0. Summed up above.
    (setf bstring (encode-data input))
    ; 5. Padding bits
    (let ((len (length bstring)))
      (setf bstring (concatenate 'string bstring
				 (padding-bits len))))
    
    ; 6. Padding Pad codewords
    (let ((len (length bstring)))
      (setf bstring (concatenate 'string bstring
				 (padding-pad-codeword (- (* (nr-data-codewords version correct)
							     8)
							  len)))))
    
    ; 7. Do error correction for above codewords
    (let ((errcobj (generate-errcobj bstring version correct)))
      (setf bstring (concatenate 'string bstring
				 (errcobj->bstring errcobj))))
    
    ; 7. Any remainder bits needed? *MODULE-CAPACITY-TABLE*
    (setf bstring (concatenate 'string bstring
			       (make-string (remainder-bits version) :initial-element #\0)))
    ; (print "=============")
    ; (print (length bstring))
    ; (print "=============")
    ; 8. TODO: Data Allocation
    bstring))
