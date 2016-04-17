;;; elens --- elens -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'glof)

(cl-defun elens:lens (g s)
  (glof:plist
   :getter g
   :setter s))

(cl-defun elens:lens-prop (n)
  (elens:lens
   (lambda (a) (glof:get a n))
   (lambda (v a) (glof:assoc a n v))))

(cl-defun elens:view (l a)
  (funcall (glof:get l :getter)
           a))

(cl-defun elens:set (l v a)
  (funcall (glof:get l :setter)
           v a))

(cl-defun elens:over (l f a)
  (elens:set l 
             (funcall f (elens:view l a))
             a))

(cl-defun elens:compose (l1 l2)
  (glof:plist
   :getter (lambda (a)
             (funcall (glof:get l1 :getter)
                      (funcall (glof:get l2 :getter) a)))
   :setter (lambda (v a)
             (funcall (glof:get l2 :setter)
                      v a))))

(provide 'elens)

;;; elens.el ends here


(cl-letf ((whole '(:b 2 :a 1 :c 9))
          (l (elens:lens-prop :a))
          (part "b")
          (part1 "a")
          (part2 "c"))
  ;; Get-Put, law 1
  (cl-equalp (elens:set l (elens:view l whole) whole)
             whole)
  ;; Put-Get, law 2
  (cl-equalp (elens:view l (elens:set l part whole))
             part)
  ;; Put-Put, law 3
  (cl-equalp (elens:set l part2 (elens:set l part1 whole))
             (elens:set l part2 whole)))

