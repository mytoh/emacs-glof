;;; glof-lookup -- glof-lookup -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(cl-defun glof:lookup (key p)
  (declare (pure t))
  (pcase p
    (`() [:nothing])
    (`(,(pred (cl-equalp key))
        ,v)
      `[:just ,v])
    ((and (pred vectorp)
        (guard (numberp key)))
     `[:just ,(seq-elt p key)])
    (`(,(pred (cl-equalp key))
        ,v . ,_)
      `[:just ,v])
    (`(,(and k (guard (not (cl-equalp key k))))
        ,_ . ,_)
      (glof:lookup key (cddr p)))))

(cl-assert (glof:lookup :a '(:a 1 :b 2))
        [:just 1])
(cl-assert (glof:lookup :b '(:a 1 :b 2))
        [:just 2])
(cl-assert (glof:lookup :c '(:a 1 :b 2))
	   [:nothing])

(provide 'glof-lookup)

;;; glof-lookup.el ends here
