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

;;;; qr-data-bstring.lisp
;;;; This file contains utilies that convert QR-INPUT to binary string (bstring,
;;;; or bitstream, we may say).
;;;; In particular, this file contains the four functions for encoding real data.
;;;;

(in-package #:cl-qrencode)

;; (0) NUMERIC mode: input data string is divided into groups of three digits.
(defun group->decimal (num-group)
  "NUM-GROUP is either 1 digit, 2 digits, or 3 digits long, encode them into decimal."
  (declare (type string num-group))
  (let ((len (length num-group))
	(result 0))
    (when (or (< len 1) (> len 3))
      (error 'qr-bad-arguments :file-name "qr-data-bstring.lisp" 
	     :function-name "group->decimal" :arguments '(num-group)
	     :how "Under NUMERIC mode, input is divided into groups of 3 (or less) digits."))
    (dotimes (idx len)
      (let ((ch (char num-group idx)))
	(when (not (digit-char-p ch))
	  (error 'qr-bad-arguments :file-name "qr-data-bstring.lisp"
		 :function-name "group->decimal" :arguments '(num-group)
		 :how "Non-digit character appears under NUMERIC mode"))
	(setf result (+ (* result 10) (- (char-code ch) (char-code #\0))))))
    result))
(defun group->bstring (num-group)
  "Encode NUM-GROUP into bstring."
  (declare (type string num-group))
  (case (length num-group)
    (3 (decimal->bstring (group->decimal num-group) 10)) ;10 bits long for 3 digits
    (2 (decimal->bstring (group->decimal num-group) 7))  ;7 bits long for 2 digits
    (1 (decimal->bstring (group->decimal num-group) 4))  ;4 bits long for 1 digits
    (t (error 'qr-bad-arguments :file-name "qr-data-bstring.lisp" 
	      :function-name "group->bstring" :arguments '(num-group)
	      :how "Under NUMERIC mode, input is diviede into groups of 3 (or less) digits."))))
(defun numeric->bstring (data)
  "Convert input DATA string to bstring under NUMERIC mode."
  (declare (type string data))
  (let ((len (length data))
	(bstring ""))
    (do ((idx 0 (incf idx 3)))
	((>= idx len) nil)
      (cond
	((> len (+ idx 2)) ; more than 3 chars left
	 (setf bstring (concatenate 'string bstring
				    (group->bstring (subseq data idx (+ idx 3))))))
	((= len (+ idx 2)) ; 2 chars left
	 (setf bstring (concatenate 'string bstring
				    (group->bstring (subseq data idx (+ idx 2))))))
	((= len (+ idx 1)) ; only 1 char left
	 (setf bstring (concatenate 'string bstring
				    (group->bstring (subseq data idx (+ idx 1))))))))
    bstring))

;; (1) ALPHANUMERIC mode: data is divided into groups of two characters, we call it "pair"
(defun char->decimal (char)
  "Only digits, capital letters, and some special characters available."
  (declare (type character char))
  (cond
    ((char<= #\0 char #\9)
     (- (char-code char) (char-code #\0)))
    ((char<= #\A char #\Z)
     (+ (- (char-code char) (char-code #\A)) 10))
    ((char= char #\Space) 36)
    ((char= char #\$) 37)
    ((char= char #\%) 38)
    ((char= char #\*) 39)
    ((char= char #\+) 40)
    ((char= char #\-) 41)
    ((char= char #\.) 42)
    ((char= char #\/) 43)
    ((char= char #\:) 44)
    (t (error 'qr-bad-arguments :file-name "qr-data-bstring.lisp"
	      :function-name "char->decimal" :arguments char
	      :how "Character not recognized under ALPHANUMERIC mode."))))
(defun pair->decimal (alpha-pair)
  "ALPHA-PAIR is either 2 char or 1 char long, encode them into decimal."
  (declare (type string alpha-pair))
  (setf alpha-pair (string-upcase alpha-pair))
  (let ((len (length alpha-pair)))
    (cond 
      ((= len 1) (char->decimal (char alpha-pair 0)))
      ((= len 2) (+ (* 45 (char->decimal (char alpha-pair 0)))
		    (char->decimal (char alpha-pair 1))))
      (t (error 'qr-bad-arguments :file-name "qr-data-bstring.lisp" 
		:function-name "pair->decimal" :arguments alpha-pair
		:how "Under ALPHANUMERIC mode, data is divided into pair of 2.")))))
(defun pair->bstring (alpha-pair)
  "Encode ALPHA-PAIR to bstring: 11 bits for two char long, 6 bits for 1 char long."
  (declare (type string alpha-pair))
  (let ((len (length alpha-pair)))
    (cond
      ((= len 2) (decimal->bstring (pair->decimal alpha-pair) 11))
      ((= len 1) (decimal->bstring (pair->decimal alpha-pair) 6))
      (t (error 'qr-bad-arguments :file-name "qr-data-bstring.lisp"
		:function-name "pair->bstring" :arguments alpha-pair
		:how "Under ALPHANUMERIC mode, data is divided into pair of 2.")))))
(defun alnum->bstring (data)
  "Convert input DATA string to bstring under ALPHANUMERIC mode."
  (declare (type string data))
  (let ((len (length data))
	(data (string-upcase data))
	(bstring ""))
    (do ((idx 0 (incf idx 2)))
	((>= idx len) nil)
      (if (>= (+ idx 1) len) ; only 1 char left
	  (setf bstring (concatenate 'string bstring
				     (pair->bstring (subseq data idx (+ idx 1)))))
	  (setf bstring (concatenate 'string bstring 
				     (pair->bstring (subseq data idx (+ idx 2)))))))
    bstring))

;; (3) BINARY mode: each value is directly encoded in 8 bit long binary representation
;;  TODO: JIS X 0201 character sets? I have to look into it.
(defun binary->bstring (data)
  "Convert input DATA string to bstring under BINARY mode."
  (declare (type string data))
  (labels ((concat (prev cur) ; PREV is the result by now
	     (concatenate 'string prev
			  (decimal->bstring (char-code cur) 8))))
    (reduce #'concat data :initial-value "")))

;; (4) KANJI mode: Shift JIS system, two bytes for a single character. JIS X 0208
(defun word->decimal (kanji-word)
  "Encode a KANJI-WORD to decimal."
  ;; TODO: This is definitely SBCL-specific, try to solve it.
  (let ((result (sb-impl::ucs-to-sjis (char-code kanji-word)))
	(subtractor 0))
    (setf subtractor (cond
		       ((<= #x8140 result #x9FFC) #x8140)
		       ((<= #xE040 result #xEBBF) #xC140)))
    (decf result subtractor)
    (setf result (+ (* (ash result -8) #xC0)
		    (boole boole-and result #xFF)))))
(defun word->bstring (kanji-word)
  "Encode a KANJI-WORD to bstring using 13 bits."
  (decimal->bstring (word->decimal kanji-word) 13))
(defun kanji->bstring (data)
  "Convert input DATA string to bstring under KANJI mode."
  (declare (type string data))
  (let ((result ""))
    (loop for kanji-word across data do
	 (setf result (concatenate 'string result
				   (word->bstring kanji-word))))
    result))