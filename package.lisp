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

;;;; package.lisp
;;;; This file contains the package defintions.
;;;;

(defpackage #:cl-qrencode
  (:use #:cl #:zpng)
  (:import-from #:zpng
		#:png
		#:data-array
		#:write-png)
  (:export #:qrencode))
	   ; #:function-pattern ; Matrix status after paint Function Patterns.
	   ; #:encoding-region))
