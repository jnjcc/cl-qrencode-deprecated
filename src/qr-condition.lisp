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

;;;; qr-condition.lisp
;;;; This file contains utilies that do error handling.
;;;;

(in-package #:cl-qrencode)

; Self-explaining, no comments needed...
(define-condition qr-condition (error)
  ((file-name :accessor file-name
	      :initarg :file-name
	      :initform nil)
   (function-name :accessor function-name
		  :initarg :function-name
		  :initform nil))
  (:report (lambda (condition stream)
	     (format stream "qr-condition occurred at `~A' in `~A'"
		     (function-name condition)
		     (file-name condition)))))

(define-condition qr-bad-arguments (qr-condition)
  ((arguments :accessor arguments
	      :initarg :arguments)
   (what :accessor what
	 :initform "Bad argument(s)")
   (how :accessor how
	:initarg :how))
  (:report (lambda (condition stream)
	     (if (and (function-name condition) (file-name condition))
		 (progn
		   (format stream "Function `~A' in file `~A' says:~%`~A': `~A'~%"
			   (function-name condition)
			   (file-name condition)
			   (what condition)
			   (arguments condition))
		   (format stream "~A~%" (how condition)))
		 (format stream "~A~%" (what condition))))))
  
(define-condition qr-not-implemented (qr-condition)
  ((mode :accessor mode
	 :initarg :mode
	 :initform "KANJI mode")
   (what :accessor what
	 :initform "not implemented yet in current version."))
  (:report (lambda (condition stream)
	     (format stream "~A ~A~%" (mode condition)
		     (what condition)))))

;; For testcases
(define-condition qr-test-failure (qr-condition)
  ((arguments :accessor arguments
	      :initarg :arguments))
  (:report (lambda (condition stream)
	     (format stream "Function ~A fails with arguments ~A"
		     (function-name condition) (arguments condition)))))