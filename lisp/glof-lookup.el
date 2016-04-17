;;; glof-lookup -- glof-lookup -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

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
      (glof:lookup key (glof:rest p)))))

(assert (glof:lookup :a '(:a 1 :b 2))
        [:just 1])
(assert (glof:lookup :b '(:a 1 :b 2))
        [:just 2])
(assert (glof:lookup :c '(:a 1 :b 2))
        [:nothing])

;;; glof-lookup.el ends here
