;;; glof-let -- glof-let -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'glof-lookup)

;; (glof:let (((a :a)
;;             (b :b)
;;             (d :d :nothing))
;;            '(:a test :b 2 :c ?\c))
;;           (list a b d))

(cl-defmacro glof:let (bindings &rest body)
  (declare (indent 1))
  (pcase bindings
    (`(nil ,_)
      `(progn ,@body))
    (_
     `(letf ((,(car (car (car bindings)))
               (pcase (glof:lookup ,(cadr (car (car bindings)))
                               ,(cadr bindings))
                 (`[:nothing]
                   (if ,(caddr (car (car bindings)))
                       ,(caddr (car (car bindings)))
                     nil))
                 (`[:just ,x]
                   x))))
            (glof:let ,(list (cdr (car bindings))
                             (cadr bindings))
              ,@body)))))

(provide 'glof-let)

;;; glof-let.el ends here
