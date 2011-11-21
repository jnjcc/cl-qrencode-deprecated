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

;;;; qr-bstring-testcase.lisp
;;;; This file contains some testcases for utilities in qr-bstring.lisp.
;;;;

(in-package #:cl-qrencode)

;;; Testcase adapted from Annex G
(defvar *input-text* "01234567")
(defvar *encode-data* "0001 0000001000 0000001100 0101011001 1000011 0000"
  "Encode result using 1-M, :NUMERIC.")
(defvar *padding-result* "00010000 00100000 00001100 01010110 01100001 10000000"
  "Padding bits.")
(defvar *pad-codeword* "00010000 00100000 00001100 01010110 01100001 10000000\
 11101100 00010001 11101100 00010001 11101100 00010001 11101100 00010001 11101100 00010001")
(defun init-bstring-global ()
  (setf *encode-data* (remove #\Space *encode-data*))  
  (setf *padding-result* (remove #\Space *padding-result*))
  (setf *pad-codeword* (remove-if #'(lambda (char)
				      (or (char= char #\Space) (char= char #\Newline)))
				  *pad-codeword*)))

(defun test-function (input function output &key (test #'string=) (call-p t))
  "CALL-P: whether or not we need to call FUNCTION."
  (let ((func-name (symbol-name function)))
       (when (not call-p)
	 (setf function #'identity))
       (when (not (funcall test (funcall function input)
			   output))
	 (error 'qr-test-failure :function-name func-name
		:arguments `(,(funcall function input) ,output)))))

(defun run-bstring-test (&key (version 1) (mode :numeric) (correct :level-m))
  (init-bstring-global)
  (let ((input (string->input *input-text* :version version
			      :mode mode :correction correct)))
    (test-function input 'encode-data (remove #\Space *encode-data*))
    (let ((bstring (encode-data input)))
      (setf bstring (concatenate 'string bstring
				 (padding-bits (length bstring))))
      (test-function bstring 'padding-bits (remove #\Space *padding-result*)
		     :call-p nil)
      (setf bstring (concatenate 'string bstring
				 (padding-pad-codeword
				  (- (* (nr-data-codewords version correct) 8) (length bstring)))))
      (test-function bstring 'padding-pad-codeword (remove #\Space *pad-codeword*)
		     :call-p nil)
      (format t "`qr-bstring.lisp' seems good as for Annex G...~%")
      bstring)))
