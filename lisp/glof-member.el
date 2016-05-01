;;; glof-member -- glof-member -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(cl-defun glof:member (n p)
  (pcase p
    (`nil [:false])
    (`(,(and x (guard (cl-equalp n x)))
        ,_ . ,_)
      [:true])
    (`(,x ,v . ,r)
      (glof:member n r))))

(cl-defun glof:not-member (n p)
  (pcase p
    (`nil [:true])
    (`(,(and x (guard (cl-equalp n x)))
        ,_ . ,_)
      [:false])
    (`(,x ,v . ,r)
     (glof:not-member n r))))

(provide 'glof-member)

;;; glof-member.el ends here
