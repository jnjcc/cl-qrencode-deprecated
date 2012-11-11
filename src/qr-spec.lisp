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

;;;; qr-spec.lisp
;;;; This file contains some data structures for the QR specifications.
;;;;

(in-package #:cl-qrencode)

;;------------------------------------------------------------------------------
;; (0) First, we need to encode the data mode we are using into bstring. There are
;;     four encodable character sets: :NUMERIC, :ALPHANUMERIC, :BINARY, :KANJI
;;------------------------------------------------------------------------------
;; TODO: Also, there should be mode indicators for Structured Append(0011), 
;;        FNC1(0101 | 1001) and ECI(0111) mode. -- Ch8.4
(defvar *mode-indicator* #("0001" "0010" "0100" "1000" "0011" "0101" "1001" "0111")
  "Four-bit identifier indicating in which mode the NEXT data sequence is 
encoded.")
(defun mode->index (mode &key (default-p nil))
  "DEFAULT-P means if we want use default value for non-recognizable MODE."
  (declare (type symbol mode))
  (case mode
    (:numeric 0)
    (:alphanumeric 1)
    (:binary 2) ; 8-bit byte data
    (:kanji 3)
    (t (if default-p
	   1
	   (error 'qr-bad-arguments :file-name "qr-spec.lisp" :function-name "mode->index"
		  :arguments '(mode) :how "Allowed mode only includes `:numeric', `:alphanumeric',
`:binary' and `:kanji'")))))
;;; Encode Mode Indicator into bstring
(defun mode->bstring (mode)
  (declare (type symbol mode))
  (let ((idx (mode->index mode :default-p t)))
    (aref *mode-indicator* idx)))

;;------------------------------------------------------------------------------
;; (1) Second, we need to encode the character count of the data into bstring,
;;     which is the so-called Character Count Indicator. The number of bits we
;;     use to encode it is decided according to the version and the mode of QRCode.
;;------------------------------------------------------------------------------
(defvar *count-indicator-bits-table*
  ; :numeric, :alpahnumeric, :binary, :kanji
  #2A((10 9  8  8)    ; Version 1 ~ 9
      (12 11 16 10)   ; Version 10 ~ 26
      (14 13 16 12))  ; Version 27 ~ 40
  "Number of bits in Character Count Indicator, which varies according to the 
mode and symbol version in use.")
(defun count-indicator-bits (version mode)
  "# of bits used to represent length of data."
  (let (i j)
    (setf i (cond 
	      ((<= 1 version 9) 0)
	      ((<= 10 version 26) 1)
	      ((<= 27 version 40) 2)
	      (t (error 'qr-bad-arguments :file-name "qr-spec.lisp"
			:function-name "count-indicator-bits" :arguments '(version)
			:how "Version ranges from 1 to 40."))))
    (setf j (mode->index mode))
    (aref *count-indicator-bits-table* i j)))

(defvar *data-capacity-table*
  ; http://www.denso-wave.com/qrcode/vertable1-e.html
  ; Version 1 ~ 40, depends on version and error correction level
  ; Table 7 ~ Table 11
  #2A((152 128 104 72)         (272 224 176 128)        (440 352 272 208)
      (640 512 384 288)        (864 688 496 368)        ; Version 1 ~ 5
      (1088 864 608 480)       (1248 992 704 528)       (1552 1232 880 688)
      (1856 1456 1056 800)     (2192 1728 1232 976)     ; Version 6 ~ 10
      (2592 2032 1440 1120)    (2960 2320 1648 1264)    (3424 2672 1952 1440)
      (3688 2920 2088 1576)    (4184 3320 2360 1784)    ; Version 11 ~ 15
      (4712 3624 2600 2024)    (5176 4056 2936 2264)    (5768 4504 3176 2504)
      (6360 5016 3560 2728)    (6888 5352 3880 3080)    ; Version 16 ~ 20
      (7456 5712 4096 3248)    (8048 6256 4544 3536)    (8752 6880 4912 3712)
      (9392 7312 5312 4112)    (10208 8000 5744 4304)   ; Version 21 ~ 25
      (10960 8496 6032 4768)   (11744 9024 6464 5024)   (12248 9544 6968 5288)
      (13048 10136 7288 5608)  (13880 10984 7880 5960)  ; Version 26 ~ 30
      (14744 11640 8264 6344)  (15640 12328 8920 6760)  (16568 13048 9368 7208)  
      (17528 13800 9848 7688)  (18488 14496 10288 7888) ; Version 31 ~ 35
      (19472 15312 10832 8432) (20528 15936 11408 8768) (21616 16816 12016 9136)
      (22496 17728 12656 9776) (23648 18672 13328 10208)); Version 36 ~ 40
  "Data bits capacity for each version and correction level, including mode indicator
and character count indicator, _not_ including error correction bits. And of course, 
for each mode, the character capacity are different.")
(defun corrlev->index (correct &key (default-p nil))
  "Get corresponding index for correct level, :level-q will be default."
  (declare (type symbol correct))
  (case correct
    (:level-l 0)
    (:level-m 1)
    (:level-q 2)
    (:level-h 3)
    (t (if default-p
	   2
	   (error 'qr-bad-arguments :file-name "qr-spec.lisp" 
		  :function-name "corrlev->index"
		  :arguments '(correct) :how "Allowded Correction Level only includes
 `:level-l',`:level-m', `:level-q' and `:level-h'")))))
(defun data-bits-capacity (version correct)
  (declare (type number version)
	   (type symbol correct))
  (if (<= 1 version 40)
      (let ((j (corrlev->index correct)))
	(aref *data-capacity-table* (- version 1) j))
      (error 'qr-bad-arguments :file-name "qr-spec.lisp" :function-name "data-bits-capacity"
	     :arguments '(version) :how "Version ranges from 1 to 40.")))

;;------------------------------------------------------------------------------
;; (3) Error Correction codewords: Table 13 ~ Table 22
;;     NOTICE: I typed, this sucks
;;------------------------------------------------------------------------------
(defvar *errc-words-table*
  ; (Version, errc-level) ==> (# of error correction codeword for each blk, # of block 1,
  ;			       # of data codewords, # of block 2, # of data codewords)
  ; :level-l :level-m :level-q :level-h
  #3A(
      ((7 1 19 0 0)      (10 1 16 0 0)    (13 1 13 0 0)    (17 1 9 0 0))     ; Version 1
      ((10 1 34 0 0)     (16 1 28 0 0)    (22 1 22 0 0)    (28 1 16 0 0))
      ((15 1 55 0 0)     (26 1 44 0 0)    (18 2 17 0 0)    (22 2 13 0 0))
      ((20 1 80 0 0)     (18 2 32 0 0)    (26 2 24 0 0)    (16 4 9 0 0))
      ((26 1 108 0 0)    (24 2 43 0 0)    (18 2 15 2 16)   (22 2 11 2 12))   ; Version 5
      ((18 2 68 0 0)     (16 4 27 0 0)    (24 4 19 0 0)    (28 4 15 0 0))
      ((20 2 78 0 0)     (18 4 31 0 0)    (18 2 14 4 15)   (26 4 13 1 14))
      ((24 2 97 0 0)     (22 2 38 2 39)   (22 4 18 2 19)   (26 4 14 2 15))
      ((30 2 116 0 0)    (22 3 36 2 37)   (20 4 16 4 17)   (24 4 12 4 13))
      ((18 2 68 2 69)    (26 4 43 1 44)   (24 6 19 2 20)   (28 6 15 2 16))   ; Version 10
      ((20 4 81 0 0)     (30 1 50 4 51)   (28 4 22 4 23)   (24 3 12 8 13))
      ((24 2 92 2 93)    (22 6 36 2 37)   (26 4 20 6 21)   (28 7 14 4 15))
      ((26 4 107 0 0)    (22 8 37 1 38)   (24 8 20 4 21)   (22 12 11 4 12))
      ((30 3 115 1 116)  (24 4 40 5 41)   (20 11 16 5 17)  (24 11 12 5 13))
      ((22 5 87 1 88)    (24 5 41 5 42)   (30 5 24 7 25)   (24 11 12 7 13))  ; Version 15
      ((24 5 98 1 99)    (28 7 45 3 46)   (24 15 19 2 20)  (30 3 15 13 16))
      ((28 10 46 1 47)   (28 10 46 1 47)  (28 1 22 15 23)  (28 2 14 17 15))
      ((30 5 120 1 121)  (26 9 43 4 44)   (28 17 22 1 23)  (28 2 14 19 15))
      ((28 3 113 4 114)  (26 3 44 11 45)  (26 17 21 4 22)  (26 9 13 16 14))
      ((28 3 107 5 108)  (26 3 41 13 42)  (30 15 24 5 25)  (28 15 15 10 16)) ; Version 20
      ((28 4 116 4 117)  (26 17 42 0 0)   (28 17 22 6 23)  (30 19 16 6 17))
      ((28 2 111 7 112)  (28 17 46 0 0)   (30 7 24 16 25)  (24 34 13 0 0))
      ((30 4 121 5 122)  (28 4 47 14 48)  (30 11 24 14 25) (30 16 15 14 16))
      ((30 6 117 4 118)  (28 6 45 14 46)  (30 11 24 16 25) (30 30 16 2 17))
      ((26 8 106 4 107)  (28 8 47 13 48)  (30 7 24 22 25)  (30 22 15 13 16)) ; Version 25
      ((28 10 114 2 115) (28 19 46 4 47)  (28 28 22 6 23)  (30 33 16 4 17))
      ((30 8 122 4 123)  (28 22 45 3 46)  (30 8 23 26 24)  (30 12 15 28 16))
      ((30 3 117 10 118) (28 3 45 23 46)  (30 4 24 31 25)  (30 11 15 31 16))
      ((30 7 116 7 117)  (28 21 45 7 46)  (30 1 23 37 24)  (30 19 15 26 16))
      ((30 5 115 10 116) (28 19 47 10 48) (30 15 24 25 25) (30 23 15 25 16)) ; Version 30
      ((30 13 115 3 116) (28 2 46 29 47)  (30 42 24 1 25)  (30 23 15 28 16))
      ((30 17 115 0 0)   (28 10 46 23 47) (30 10 24 35 25) (30 19 15 35 16))
      ((30 17 115 1 116) (28 14 46 21 47) (30 29 24 19 25) (30 11 15 46 16))
      ((30 13 115 6 116) (28 14 46 23 47) (30 44 24 7 25)  (30 59 16 1 17))
      ((30 12 121 7 122) (28 12 47 26 48) (30 39 24 14 25) (30 22 15 41 16)) ; Version 35
      ((30 6 121 14 122) (28 6 47 34 48)  (30 46 24 10 25) (30 2 15 64 16))
      ((30 17 122 4 123) (28 29 46 14 47) (30 49 24 10 25) (30 24 15 46 16))
      ((30 4 122 18 123) (28 13 46 32 47) (30 48 24 14 25) (30 42 15 32 16))
      ((30 20 117 4 118) (28 40 47 7 48)  (30 43 24 22 25) (30 10 15 67 16))
      ((30 19 118 6 119) (28 18 47 31 48) (30 34 24 34 25) (30 20 15 61 16)) ; Version 40
      ))
(defun errc-words (version correct idx)
  "IDX is 0 ~ 4, their corresponding meanings are explained above."
  (let ((j (corrlev->index correct)))
    (aref *errc-words-table* (- version 1) j idx)))
(defun nr-errc-codewords (version correct)
  (declare (type number version)
	   (type symbol correct))
  (errc-words version correct 0))
(defun nr-block (version correct blk)
  (assert (or (= blk 1) (= blk 2)))
  (errc-words version correct (- (* blk 2) 1)))
(defun nr-data-codewords (version correct blk)
  (declare (type number version)
	   (type symbol correct))
  (assert (or (= blk 1) (= blk 2)))
  (errc-words version correct (* 2 blk)))
(defun nr-codewords (version correct)
  "Both data codewords and error correction codewords."
  (declare (type number version)
	   (type symbol correct))
  (+ (nr-errc-codewords version correct)
     (nr-data-codewords version correct)))

;;------------------------------------------------------------------------------
;; (4) We now must choose the best mask pattern.
;;     First, we make an empty matrix of the size modules: Table 1
;;------------------------------------------------------------------------------
(defvar *module-capacity-table*
  #2A(
      (21  202  31 208   26   0) (25  235  31 359   44   7)
      (29  243  31 567   70   7) (33  251  31 807   100  7)
      (37  259  31 1079  134  7) (41  267  31 1383  172  7)
      (45  390  67 1568  196  0) (49  398  67 1936  242  0)
      (53  406  67 2336  292  0) (57  414  67 2768  346  0) ; Version 10
      (61  422  67 3232  404  0) (65  430  67 3728  466  0)
      (69  438  67 4256  532  0) (73  611  67 4651  581  3)
      (77  619  67 5243  655  3) (81  627  67 5867  733  3)
      (85  635  67 6523  815  3) (89  643  67 7211  901  3)
      (93  651  67 7931  991  3) (97  659  67 8683  1085 3) ; Version 20
      (101 882  67 9252  1156 4) (105 890  67 10068 1258 4)
      (109 898  67 10916 1364 4) (113 906  67 11796 1474 4)
      (117 914  67 12708 1588 4) (121 922  67 13652 1706 4)
      (125 930  67 14628 1828 4) (129 1203 67 15371 1921 3)
      (133 1211 67 16411 2051 3) (137 1219 67 17483 2185 3) ; Version 30
      (141 1227 67 18587 2323 3) (145 1235 67 19723 2465 3)
      (149 1243 67 20891 2611 3) (153 1251 67 22091 2761 3)
      (157 1574 67 23008 2876 0) (161 1582 67 24272 3034 0)
      (165 1590 67 25568 3196 0) (169 1598 67 26896 3362 0)
      (173 1606 67 28256 3532 0) (177 1614 67 29648 3706 0)) ; Version 40
  "Number of modules(as version increased, 4 modules added) A | Function pattern
modules B | Format and Version information modules C | Data modules(A^2-B-C) | Data 
capacity codewords(including error correction) | Remainder bits.")
(defun module-capacity (version idx)
  (when (or (not (<= 1 version 40))
	    (not (<= 0 idx 5)))
    (error 'qr-bad-arguments :file-name "qr-spec.lisp"
	   :function-name "module-capacity" :arguments '(version idx)
	   :how "VERSION ranges from 1 to 40; IDX ranges from 0 to 5."))
  (aref *module-capacity-table* (- version 1) idx))
(defun matrix-modules (version)
  "Overall modules: (MATRIX-MODULES) * (MATRIX-MODULES)."
  (if (<= 1 version 40)
      (module-capacity version 0)
      (error 'qr-bad-arguments :file-name "qr-spec.lisp"
	     :function-name "matrix-modules"
	     :arguments version
	     :how "Version ranges from 1 to 40.")))
(defun remainder-bits (version)
  "Remainder bits to be added after the error correction codeword."
  (if (<= 1 version 40)
      (module-capacity version 5)
      (error 'qr-bad-arguments :file-name "qr-spec.lisp"
	     :function-name "remainder-bits" :arguments version
	     :how "Version ranges from 1 to 40.")))

;; Table E.1
(defvar *align-coord-table*
  #2A(
      (0  ())                       (1  (6 18))                   (1  (6 22))
      (1  (6 26))                   (1  (6 30))                   (1  (6 34))
      (6  (6 22 38))                (6  (6 24 42))                (6  (6 26 46))
      (6  (6 28 50))                (6  (6 30 54))                (6  (6 32 58))
      (6  (6 34 62))                (13 (6 26 46 66))             (13 (6 26 48 70))
      (13 (6 26 50 74))             (13 (6 30 54 78))             (13 (6 30 56 82))
      (13 (6 30 58 86))             (13 (6 34 62 90))             (22 (6 28 50 72 94))
      (22 (6 26 50 74 98))          (22 (6 30 54 78 102))         (22 (6 28 54 80 106))
      (22 (6 32 58 84 110))         (22 (6 30 58 86 114))         (22 (6 34 62 90 118))
      (33 (6 26 50 74 98 122))      (33 (6 30 54 78 102 126))     (33 (6 26 52 78 104 130))
      (33 (6 30 56 82 108 134))     (33 (6 34 60 86 112 138))     (33 (6 30 58 86 114 142))
      (33 (6 34 62 90 118 146))     (46 (6 30 54 78 102 126 150)) (46 (6 24 50 76 102 128 154))
      (46 (6 28 54 80 106 132 158)) (46 (6 32 58 84 110 136 162)) (46 (6 26 54 82 110 138 166))
      (46 (6 30 58 86 114 142 170)))
  "# of Alignment Patterns, row/column coordinates of center modules.")
(defun valid-center-p (x y modules)
  "The Alignment Center is not in the Finder Patterns."
  (not (or (and (<= 0 x 8) (<= 0 y 8)) ; In the upper left finder pattern
	   (and (<= 0 x 8)
		(<= (- modules 8) y (- modules 1))) ; In the upper right finder pattern
	   (and (<= (- modules 8) x (- modules 1))
		(<= 0 y 8)))))
;; Annex E
(defun align-centers (version)
  "Get the centers of the Alignment Patterns."
  (let* ((modules (matrix-modules version))
	 (coords (aref *align-coord-table* (- version 1) 1))
	 (len (length coords))
	 (centers ()))
    (dotimes (i len)
      (loop for j from i to (- len 1) do
	   (let ((x (elt coords i))
		 (y (elt coords j)))
	     (when (valid-center-p x y modules)
	       (setf centers (append centers (list `(,x ,y)))))
	     (when (not (= x y))
	       (when (valid-center-p y x modules)
		 (setf centers (append centers (list `(,y ,x)))))))))
    centers))
    
(defun mask-condition (indicator)
  "There are 8 mask patterns, get the mask generation condition, modules which
meet the condition are reversed."
  (declare (type number indicator))
  (lambda (x y)
    (case indicator
      (0 (= (mod (+ x y) 2) 0))
      (1 (= (mod x 2) 0))
      (2 (= (mod y 3) 0))
      (3 (= (mod (+ x y) 3) 0))
      (4 (= (mod (+ (floor x 2)
		    (floor y 3))
		 2) 0))
      (5 (= (+ (mod (* x y) 2)
	       (mod (* x y) 3)) 0))
      (6 (= (mod (+ (mod (* x y) 2)
		    (mod (* x y) 3))
		 2) 0))
      (7 (= (mod (+ (mod (* x y) 3)
		    (mod (+ x y) 2))
		 2) 0))
      (t (error 'qr-bad-arguments :file-name "qr-spec.lisp" :function-name "masker"
		:arguments '(indicator)
		:how "There are only 8 mask patterns, so indicator ranges from 0 to 7.")))))

;;------------------------------------------------------------------------------
;; (5) Now, We paint the Format Information & Version Information in the matrix modules
;;------------------------------------------------------------------------------
(defvar *errc-indicator* #("01" "00" "11" "10")
  "Table 25: :LEVEL-L, :LEVEL-M, :LEVEL-Q, :LEVEL-H")
(defvar *mask-pattern-ref* #("000" "001" "010" "011" "100" "101" "110" "111")
  "Table 23: Mask Pattern Reference and generation conditions.")
(defvar *format-generator* '((0 . 10) (0 . 8) (0 . 5) (0 . 4) (0 . 2) (0 . 1) (0 . 0)) ; Note: here, 0 means a^0
  "Format Information Generator Polynomial <x^10 + x^8 + x^5 + x^4 + x^2 + x + 1>.")

(defvar *version-generator* '((0 . 12) (0 . 11) (0 . 10) (0 . 9) (0 . 8) (0 . 5) (0 . 2) (0 . 0))
  "Version Information Generator Polynomial <x^12 + x^11 + x^10 + x^9 + x^8 + x^5 + x^2 + 1>.")