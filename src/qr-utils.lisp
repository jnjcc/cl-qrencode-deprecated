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

;;;; qr-utils.lisp
;;;; This file contains some basic utilies.
;;;;

(in-package #:cl-qrencode)

;; Use it or not, I list it here.
(defmacro while (test &rest body)
  "I always wonder why there is no WHILE in Common Lisp."
  `(do ()
       ((not ,test) nil)
     ,@body))


;; Adapted from P. Norvig's PAIP
(defvar *dbg-ids* nil)
(defun sdebug (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))
(defun undebug (&rest ids)
  (setf *dbg-ids* (if (null ids)
		      nil
		      (set-difference *dbg-ids* ids))))
(defun dbg (id format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))