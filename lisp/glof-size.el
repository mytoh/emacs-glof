;;; glof-size -- glof-size -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(cl-defun glof:size (p)
  (pcase p
    (`nil 0)
    (`(,k ,v) 1)
    (`(,k ,v . ,r)
      (+ 1 (glof:size r)))))

(provide 'glof-size)

;;; glof-size.el ends here
