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

;;;; qr-blocks.lisp
;;;; This file contains

(in-package #:cl-qrencode)

(defclass qr-block ()
  ((block-id :initarg :block-id :accessor block-id) ; 1 or 2?
   (data-codewords :initarg :data-codewords :accessor data-codewords)
   (errc-codewords :initarg :errc-codewords :initform nil :accessor errc-codewords)))

(defun bstring->blocks (bstring version correct)
  (let ((blocks nil))
    ; block 1 & 2
    (loop for blk from 1 to 2 do
	 (let ((nr-blk (nr-block version correct blk))
	       (sz-blk (* (nr-data-codewords version correct blk) 8))
	       (data ""))
	   (loop for i from 1 to nr-blk do
		(setf data (subseq bstring 0 sz-blk))
		(setf bstring (subseq bstring sz-blk))
		(let ((blkobj (make-instance 'qr-block :data-codewords data
					     :block-id blk)))
		  (push blkobj blocks)))))
    (reverse blocks)))

(defun block-errc (ablock version correct)
  (let ((bstring (data-codewords ablock))
	(blk (block-id ablock)))
    (setf (errc-codewords ablock) (errcobj->bstring 
				   (generate-errcobj bstring version correct blk)))
    (fresh-line)))

(defun do-errc (blocks version correct)
  (loop for ablock in blocks do
       (block-errc ablock version correct)))

(defun blocks->bstring (blocks version correct)
  (let ((bstring "")
	(nr-blk (length blocks))
	(idx (max (nr-data-codewords version correct 1)
		  (nr-data-codewords version correct 2)))
	(idx2 (nr-errc-codewords version correct)))

    (loop for i from 0 to (- idx 1) do
	 (loop for j from 1 to nr-blk do
	      (and (< (* 8 i) (length (data-codewords (nth (- j 1) blocks))))
		   (setf bstring (concatenate 'string bstring
					      (subseq (data-codewords (nth (- j 1) blocks))
						      (* 8 i)
						      (* 8 (+ i 1))))))))

    (loop for i from 0 to (- idx2 1) do
	 (loop for j from 1 to nr-blk do
	      (setf bstring (concatenate 'string bstring
					 (subseq (errc-codewords (nth (- j 1) blocks))
						 (* 8 i)
						 (* 8 (+ i 1)))))))
    bstring))