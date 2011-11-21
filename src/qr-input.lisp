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

;;;; qr-input.lisp
;;;; This file contains utilities that convert string to QR-INPUT class.
;;;;

(in-package #:cl-qrencode)

(defclass qr-input ()
   ; VERSION: 1~40
  ((version :initarg :version :initform 1 :accessor version)
   ; MODE indicator: :numberic, :alphanumeric, :binary, or :kanji
   (mode :initarg :mode :initform :alphanumeric :reader mode)
   ; Correction: :level-l :level-m :level-q :level-h
   (correction :initarg :correction :initform :level-q :reader correction)
   (data :initarg :data :reader data)))

(defmethod print-object ((input qr-input) stream)
  (fresh-line)
  (format stream ">>>>>>>>>>>>>>>>>>>>>>>>>>>>~%")
  (format stream "Object of class QR-INPUT:~%~T")
  (format stream "Data:~T`~A'~%~TVersion:~T~A~%~TMode:~T~A~%~TCorrection Level:~T~A"
	  (data input) (version input) (mode input) (correction input))
  (format stream "~%<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%"))

(defun string->input (text &key (version 1) (mode :alphanumeric) (correction :level-q))
  "Put the input text into the QR-INPUT instance."
  (declare (type string text) (type symbol mode correction))
  (let ((size (length text)))
    (when (or (>= size (- (+ (errc-words version correction 3) 
			     (* (+ (errc-words version correction 1)
				   (errc-words version correction 3))
				(errc-words version correction 2)))
			  3))
	      (or (< version 1) (> version 40)))
      (setf version (adjust-version size version correction))))
  (let ((input (make-instance 'qr-input :data text
			      :version version :mode mode :correction correction)))
    input))
(defun adjust-version (size version correction)
  (let ((old version))
    (loop for ver from 1 to 40 do
	 (when (< size (- (+ (errc-words ver correction 3)
			     (* (+ (errc-words ver correction 1)
				   (errc-words ver correction 3))
				(errc-words ver correction 2)))
			  3))
	   (setf version ver)
	   (return)))
    (if (= old version)
	(error "Not able to hold that many codewords.")
	version)))