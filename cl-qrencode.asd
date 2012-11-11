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

;;;; cl-qrencode.asd
;;;; This file contains

; #-(or sbcl)
; (error "Unfortunately, this package only works under SBCL for now.")

(asdf:defsystem #:cl-qrencode
  :name    "cl-qrencode"
  :description "QR Code encoder in pure Common Lisp."
  :author  "johnc <jnjcc@live.com>"
  :version "1.0.2"
  :maintainer "johnc <jnjcc@live.com>"
  :licence "GPL"
  :depends-on (#:zpng #:babel)
  :components ((:file "package")
	       (:module "src"
			:components ((:file "qr-utils")
				     (:file "qr-condition")
				     (:file "qr-spec")
				     (:file "qr-input")
				     (:file "qr-data-bstring")
				     (:file "qr-bstring")
				     (:file "qr-errc")
				     (:file "qr-encode")
				     (:file "qr-blocks")
				     (:file "qr-mask")
				     (:file "qr-image")))
	       (:module "tests"
			:components ((:file "qr-bstring-testcase")
				     (:file "qr-encode-testcase")
				     (:file "qr-errc-testcase")))))