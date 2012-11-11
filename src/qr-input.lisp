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
   (mode :initarg :mode :initform :binary :reader mode)
   ; Correction: :level-l :level-m :level-q :level-h
   (correction :initarg :correction :initform :level-q :reader correction)
   ; Raw input data
   (data :initarg :data :reader data)
   ; Data encoded into bstring, depends on MODE only
   (data-bstring :initarg :data-bstring :accessor data-bstring)))

(defmethod print-object ((input qr-input) stream)
  (fresh-line)
  (format stream ">>>>>>>>>>>>>>>>>>>>>>>>>>>>~%")
  (format stream "Object of class QR-INPUT:~%~T")
  (format stream "Data:~T`~A'~%~TVersion:~T~A~%~TMode:~T~A~%~TCorrection Level:~T~A"
	  (data input) (version input) (mode input) (correction input))
  (format stream "~%<<<<<<<<<<<<<<<<<<<<<<<<<<<<~%"))

(defmethod encode-input ((input qr-input))
  (with-slots (version mode data data-bstring) input
    (setf data-bstring (data->bstring data mode))
    ; Terminator
    (setf data-bstring (concatenate 'string data-bstring
				    "0000")))
  input)

(defmethod adjust-version ((input qr-input) data-len)
  "Assume we can find a version to hold all data"
  (with-slots (version mode correction data data-bstring) input
    (loop for ver from version to 41 do
	 (when (= ver 41)
	   (setf version 41)
	   (return))
         ; mode indicator length, character count indicator length
	 (let ((mil 4)
	       (cil (count-indicator-bits ver mode)))
	   (when (<= (* (ceiling (+ mil cil data-len) 8) 8)
		     (data-bits-capacity ver correction))
	     (unless (= version ver)
	       (dbg :qr-input-core "Version reset from ~A to ~A~%" version ver)
	       (setf version ver))
	     (return))))))

(defun string->input (text &key (version 1) (mode :binary) (correction :level-q))
  "Put the input text into the QR-INPUT instance, with version adjusted"
  (declare (type string text) (type symbol mode correction))
  (setf version (max version 1))
  (setf version (min version 40))
  (let ((input (make-instance 'qr-input :data text
			      :version version :mode mode :correction correction)))
    (let ((len (length (data-bstring (encode-input input)))))
      (adjust-version input len))
    input))