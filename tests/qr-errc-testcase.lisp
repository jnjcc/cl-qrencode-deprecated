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

;;;; qr-errc-testcase.lisp
;;;; This file contains some testcase to Reed-Solomon Error Correction
;;;;

(in-package #:cl-qrencode)

;; Version 1.3.2?! That was a joke...

; INPUT:  (coff1 exp1) (coff2 exp2), ...
; OUTPUT: ((coff1 . exp1) (coff2 . exp2) ...)
(defmacro make-message-poly-1 (&rest parts)
  (let ((part (gensym)))
    `(loop for ,part in ',parts collect (cons (first ,part) (second ,part)))))
; generator polynomial: the first coff1 must be 1
(defmacro make-generator-poly-1 (&rest parts)
  (if (not (= (car (first parts)) 0))
      (error 'qr-bad-arguments :file-name "qr-errc.lisp" :function-name "make-generator-poly"
	     :arguments '(parts) :how "The first cofficient of generator polynomial must be 1.")
      (let ((part (gensym)))
	`(loop for ,part in ',parts collect (cons (first ,part) (second ,part))))))

; "HELLO WORLD": Version 1, :level-q, alphanumeric mode
(defvar *msgpoly-helloworld* (make-message-poly-1 (32 25) (91 24) (11 23) (120 22)
						    (209 21) (114 20) (220 19) (77 18)
						    (67 17) (64 16) (236 15) (17 14)
						    (236 13)))
(defvar *genpoly-13* (make-generator-poly-1 (0 13) (74 12) (152 11) (176 10)
					    (100 9) (86 8) (100 7) (106 6)
					    (104 5) (130 4) (218 3) (206 2)
					    (140 1) (78 0)))
; "ABCDE123": Version 1, :level-h, alphanumeric mode
(defvar *msgpoly-abcde123* (make-message-poly-1 (32 25) (65 24) (205 23)
						(69 22) (41 21) (220 20)
						(46 19) (128 18) (236 17)))
(defvar *genpoly-17* (make-generator-poly-1 (0 17) (43 16) (139 15) (206 14)
					    (78 13) (43 12) (239 11) (123 10)
					    (206 9) (214 8) (147 7) (24 6)
					    (99 5) (150 4) (39 3) (243 2)
					    (163 1) (136 0)))
;;; Testcase adapted from Annex G
(defvar *errc-result* "00010000 00100000 00001100 01010110 01100001 10000000 11101100 00010001\
11101100 00010001 11101100 00010001 11101100 00010001 11101100 00010001 10100101 00100100\
11010100 11000001 11101101 00110110 11000111 10000111 00101100 01010101")
(defun init-errc-global ()
  (setf *errc-result* (remove-if #'(lambda (char)
				     (or (char= char #\Newline) (char= char #\Space)))
				 *errc-result*)))

(defun run-errc-test (&key (version 1) (mode :numeric) (correct :level-m))
  (init-errc-global)
  (let* ((bstring (run-bstring-test :version version :mode mode :correct correct))
	 (errcobj (generate-errcobj bstring version correct)))
    (setf bstring (concatenate 'string bstring
			       (errcobj->bstring errcobj)))
    (when (not (string= *errc-result* bstring))
      (error 'qr-test-failure :function-name (symbol-name 'errcobj->bstring)
	     :arguments `(,bstring ,*errc-result*)))
    (format t "`qr-errc.lisp' seems good as for Annex G...~%")
    bstring))