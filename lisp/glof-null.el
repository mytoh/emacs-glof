;;; glof-null -- glof-null -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(cl-defun glof:null (p)
  (pcase p
    (`nil [:true])
    (_ [:false])))

(provide 'glof-null)

;;; glof-null.el ends here
