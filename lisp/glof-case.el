;;; glof-case -- glof-case -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

;; (glof:case '(:a (:A B) :b 2 :c 3)
;;   (:a `(,a ,b)
;;       a)
;;   (:b 2
;;       :two))

(cl-defmacro glof:case (expr &rest cases)
  (declare (indent 1))
  (if cases
      (cl-letf ((ufound (gensym "Glof"))
                (fcase (car cases)))
        `(cl-letf ((,ufound (glof:lookup ,(car fcase) ,expr)))
           (if (eq :just (seq-elt ,ufound 0))
               (pcase (seq-elt ,ufound 1)
                 (,(seq-elt fcase 1)
                   ,@(cdr (cdr fcase)))
                 (_
                  (glof:case ,expr
                    ,@(cdr cases))))
             (glof:case ,expr
               ,@(cdr cases)))))
    nil))


;;; glof-case.el ends here
