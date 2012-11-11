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

;;; The overall function to encode qr-input to bstring.
(defun input->bstring (input)
  (declare (type qr-input input))
  (let ((bstring ""))
    (with-slots (version mode data-bstring (correct correction)) input
      (setf bstring (concatenate 'string bstring
				 (mode->bstring mode)
				 (decimal->bstring (length (data input))
						   (count-indicator-bits version mode))
				 data-bstring))

      ; 5. Padding bits
      (let ((len (length bstring)))
	(setf bstring (concatenate 'string bstring
				   (padding-bits len))))
      ; 6. Padding Pad codewords
      (let ((len (length bstring)))
	(setf bstring (concatenate 'string bstring
				   (padding-pad-codeword (- (data-bits-capacity version correct)
							    len)))))

      (let ((blocks (bstring->blocks bstring version correct)))
	(do-errc blocks version correct)
	(setf bstring (blocks->bstring blocks version correct)))
    
      ; 7. Any remainder bits needed? *MODULE-CAPACITY-TABLE*
      (setf bstring (concatenate 'string bstring
				 (make-string (remainder-bits version) :initial-element #\0))))
    
    bstring))
