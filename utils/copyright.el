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

;;;; copyright.el
;;;; This file contains

;; Version 1.3.2?! That was a joke...

(setq auto-insert-directory (expand-file-name "~/.emacs.d/templates/"))
(setq auto-insert-query nil) ;; Don't ask, just do the auto insertion
(define-auto-insert 'lisp-mode "lisp.tpl") ;; ~/.emacs.d/templates/lisp.tpl
(add-hook 'find-file-hooks 'auto-insert)

(defvar *prefix* ";;;;"
  "Of course, \* notation is not common in Emacs Lisp, but, let's use it anyway.")
(defun qr-add-file-name ()
  "Go to the comment line start with *PREFIX* and add the current buffer name."
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	(widen)
	(goto-char (point-min))
	(search-forward *prefix*)
	(let ((fname (file-name-nondirectory buffer-file-name)))
	  (when (not (looking-at fname))
	    (delete-region (point) (line-end-position))
	    (insert (concat " " fname))))))))