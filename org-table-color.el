;;; org-table-color.el --- Add color to your org-mode table cells -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Colin Woodbury
;;
;; Author: Colin Woodbury <colin@fosskers.ca>
;; Maintainer: Colin Woodbury <colin@fosskers.ca>
;; Created: March 10, 2022
;; Modified: March 10, 2022
;; Version: 1.0.0
;; Keywords: data faces lisp
;; Homepage: https://github.com/fosskers/org-table-color
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Add color to your org-mode table cells.
;;
;;  The principal function is `org-table-color', which can be passed a function
;;  that produces a face for each table cell. See
;;  `org-table-color--color-by-correlation' for an example.
;;
;;  Or call a built-in styling function like
;;  `org-table-color-correlation-matrix' if you're not concerned with
;;  customisation.
;;
;;  See the Github README for full instructions.
;;
;;; Code:

(require 'org-table)

;;;###autoload
(defun org-table-color-auto ()
  "Color table at point based on `:color' header property.

If `:color' is unset use `org-table-color--color-by-correlation' as
default.
Set the `:color' property with the same style used for org source block header arguments:

#+HEADER: :color 'my-coloring-function :color-min 1"
  (interactive)
  (let* ((headers (car (plist-get (car (alist-get 'table (org-element-lineage (org-element-at-point)))) :header)))
         (args (org-babel-parse-header-arguments headers))
         (color-func
          (or (alist-get :color args) #'org-table-color--color-by-correlation))
         (color-min (alist-get :color-min args))
         (color-min-row (or (alist-get :color-row args) color-min))
         (color-min-col (or (alist-get :color-col args) color-min)))
    (org-table-color color-func color-min-row color-min-col)))

;;;###autoload
(defun org-table-color (get-face &optional min-row min-col)
  "Color the 'org-mode' table at 'point', given a GET-FACE function.

GET-FACE must accept a single numerical argument (the value of
the cell) and return either a plist representing a face or nil.
When nil, no styling of that cell will occur. Further, no styling
will occur if the cell value is not a number.

See `org-table-color--color-by-correlation' for an example."
  (let* ((lisp (org-table-to-lisp))
         (rows (length lisp))
         (cols (length (car lisp)))
         (min-row (or min-row 2))
         (min-col (or min-col 2)))
    (save-excursion
      (mapc (lambda (x) (mapc (lambda (y) (org-table-color--color-cell get-face x y))
                              (number-sequence min-row rows)))
            (number-sequence min-col cols)))))

;;;###autoload
(defun org-table-color-correlation-matrix ()
  "Color the 'org-mode' table at 'point' that represents a Correlation Matrix."
  (interactive)
  (org-table-color #'org-table-color--color-by-correlation))

(defun org-table-color--color-cell (get-face x y)
  "Color the cell via a GET-FACE function at the given X and Y coordinates."
  (org-table-goto-line y)
  (org-table-goto-column x)
  (when-let* ((cell (org-table-get y x))
              (face (funcall get-face cell))
              (over (make-overlay (point)
                                  (progn (org-table-end-of-field 1)
                                         (point)))))
    (overlay-put over 'face face)))

(defun org-table-color--color-by-correlation (cell)
  "Color a table cell CELL value assuming it's from a correlation matrix.
Yields a plist that represents a face."
  (let ((num (string-to-number cell)))
  (cond ((>= num 0.5) '(:foreground "black" :background "green"))
        ((>= num 0.3) '(:foreground "black" :background "#90EE90"))
        ((<= num -0.5) '(:foreground "black" :background "red"))
        ((<= num -0.3) '(:foreground "black" :background "orange")))))

(provide 'org-table-color)
;;; org-table-color.el ends here
