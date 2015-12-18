;;; glof -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
(require 'subr-x)

(cl-defun glof:plist (&rest kvs)
  (declare (pure t))
  (apply #'list kvs))

(cl-defun glof:conj (&rest plists)
  (declare (pure t))
  (pcase plists
    (`(nil nil) (glof:empty))
    (`(nil ,x) x)
    (`(,p nil) p)
    (`(,p ,x)
      (glof:conj (glof:rest p)
              (cl-letf ((found (glof:contains-p
                                x (glof:key (glof:first p)))))
                (if found
                    x
                  (seq-concatenate 'list
                                   (glof:first p)
                                   x)))))
    (`(,p ,x nil)
      (glof:conj p x))
    (`(,p ,x ,x2 . ,xs)
      (glof:conj (glof:conj p x)
              x2 xs))))


(cl-defun glof:key (p)
  (declare (pure t))
  (pcase p
    (`(,k ,_v)
      k)))

(cl-defun glof:val (p)
  (declare (pure t))
  (pcase p
    (`(,_k ,v)
      v)))

(cl-defun glof:get (p key &optional (default nil))
  (declare (pure t))
  (pcase p
    (`() (glof:empty))
    (`(,(pred (cl-equalp key))
        ,v)
      v)
    (`(,(and k (guard (not (cl-equalp key k))))
        ,_)
      default)
    (`(,(pred (cl-equalp key))
        ,v . ,_)
      v)
    (`(,(and k (guard (not (cl-equalp key k))))
        ,_ . ,_)
      (glof:get (glof:rest p) key default))))

(cl-defun glof:first (p)
  (declare (pure t))
  (pcase-let ((`(,key ,value . ,_)
                p))
    `(,key ,value)))

(cl-defun glof:second (p)
  (declare (pure t))
  (glof:first (glof:rest p)))

(cl-defun glof:rest (p)
  (declare (pure t))
  (cddr p))

(cl-defun glof:keys (p)
  (declare (pure t))
  (pcase p
    (`() (glof:empty))
    (`(,x . ,_)
      (thread-last p
        glof:rest
        glof:keys
        (cons x)))))

(cl-defun glof:vals (p)
  (declare (pure t))
  (pcase p
    (`() (glof:empty))
    (`(,_ ,x . ,_)
      (thread-last p
        glof:rest
        glof:vals
        (cons x)))))

(cl-defun glof:assoc (plist key value &rest kvs)
  (declare (pure t))
  (pcase kvs
    (`()
      (cl-labels ((rec (p flag)
                    (pcase p
                      (`()
                        (if (null flag)
                            (list key value)
                          (glof:empty)))
                      (`(,(pred (cl-equalp key)) . ,_)
                        (cl-list* key value (rec (glof:rest p) t)))
                      (_
                       (seq-concatenate 'list
                                        (glof:first p)
                                        (rec (glof:rest p) flag))))))
        (rec plist nil)))
    (`(,k ,v)
      (glof:assoc (glof:assoc plist key value)
               k  v))
    (`(,k ,v . ,rkv)
      (apply #'glof:assoc (glof:assoc plist key value)
             k v rkv))))

(cl-defun glof:assoc-in (p ks v)
  (declare (pure t))
  (pcase ks
    ((pred glof::seq-empty-p) p)
    ((seq k
          (pred  glof::seq-empty-p))
     
     (glof:assoc p k v))
    ((seq k &rest kr)
     (glof:assoc p k (glof:assoc-in (glof:get p k) kr v)))))

(cl-defun glof:dissoc (p &rest keys)
  (declare (pure t))
  (pcase keys
    (`()
      p)
    (`(nil . ,ks)
      (apply #'glof:dissoc p ks))
    (`(,(guard (seq-empty-p p)))
      (glof:empty))
    (`(,(pred (cl-equalp (glof:key (glof:first p)))))
      (glof:rest p))
    (`(,(and k
             (guard (not (cl-equalp k (glof:key (glof:first p)))))))
      (append
       (glof:first p)
       (glof:dissoc (glof:rest p) k)))
    (`(,k nil)
      (glof:dissoc p k))
    (`(,k ,k2 . ,ks)
      (glof:dissoc (glof:dissoc p k)
                k2 ks))))

(cl-defun glof:alistify (p)
  (declare (pure t))
  (glof:map
   (pcase-lambda (`(,k ,v)) (cons k v))
   p))

(cl-defun glof:sort-by (p)
  (declare (pure t))
  (cl-letf* ((alist (glof:alistify p))
             (sorted
              (sort alist
                    (pcase-lambda (`(,a . ,_) `(,b . ,_))
                        (string< a b)))))
    (seq-mapcat
     (pcase-lambda (`(,k . ,v)) (list k v))
     sorted)))

;; [[http://www.cs.toronto.edu/~dianaz/Example_LispPart1.html]]
;; (defun glof:sort (p)
;;   (pcase p
;;     (`() '())
;;     (`(,_ ,_) p)
;;     ((guard (string< (glof:key (glof:first p))
;;                      (glof:key (glof:second p))))
;;      (glof::check-again (glof:conj (glof:first p)
;;                                      (glof:sort (glof:rest p)))))
;;     (_
;;      (glof::check-again
;;       (glof:conj (glof:second p)
;;                   (glof:sort (glof:conj (glof:first p)
;;                                           (glof:rest (glof:rest p)))))))))

;; (defun glof::check-again (p)
;;   (if (string< (glof:key (glof:first p))
;;                (glof:key (glof:second p)))
;;       p
;;     (glof:sort p)))

(cl-defun glof:map (f p)
  (glof:foldr
   (lambda (a l)
     (cons (funcall f a) l))
   (glof:empty)
   p))

(cl-defun glof:select-keys (p keys)
  (declare (pure t))
  (seq-mapcat
   (pcase-lambda ((and k (pred (glof:contains-p p))))
       (if-let ((found (glof:get p k)))
           (list k found)
         nil))
   keys))

(cl-defun glof:last (p)
  (declare (pure t))
  (pcase p
    (`() (glof:empty))
    (`(,k ,v) (glof:plist k v))
    (_ (glof:last (glof:rest p)))))

(cl-defun glof:foldr (f z p)
  (declare (pure t))
  (pcase p
    (`() z)
    (_
     (funcall f (glof:first p)
              (glof:foldr f z
                       (glof:rest p))))))

(cl-defun glof:foldl (f z p)
  (declare (pure t))
  (pcase p
    (`() z)
    (_
     (glof:foldl f
              (funcall f z (glof:first p))
              (glof:rest p)))))

(cl-defun glof:unfold (p h ns x)
  (pcase (funcall p x)
    (`t ())
    (_
     (glof:conj (funcall h x)
             (glof:unfold p h ns (funcall ns x))))))

(cl-defun glof:entry-p (p)
  (declare (pure t))
  (= 2 (seq-length p)))

(cl-defun glof:contains-p (p k)
  (declare (pure t))
  (pcase p
    (`() (glof:empty))
    (`(,(pred (cl-equalp k))
        . ,_)
      t)
    (`(,(and ki
             (guard (not (cl-equalp k ki))))
        ,_ . ,kvr)
      (glof:contains-p kvr k))))

(cl-defun glof:get-in (p ks &optional (default nil))
  (declare (pure t))
  (pcase ks
    ((pred glof::seq-empty-p)
     p)
    ((seq (and k
               (pred (glof:contains-p p)))
          (pred glof::seq-empty-p))
     (glof:get p k))
    ((seq (and k
               (guard (not (glof:contains-p p k)))))
     default)
    ((seq (and k
               (pred (glof:contains-p p)))
          &rest kr)
     (glof:get-in (glof:get p k)
               kr default))
    ((seq (and k
               (guard (not (glof:contains-p p k))))
          &rest _kr)
     default)))

(cl-defun glof:update (p k f)
  (declare (pure t))
  (glof:assoc p k
           (funcall f (glof:get p k))))

(cl-defun glof:zipmap (keys vals)
  ;; [[https://www.youtube.com/watch?v=n7aE6k8o_BU]]
  (declare (pure t))
  (pcase `(,keys,vals)
    (`(nil nil) nil)
    (`(nil (,_ . ,_)) nil)
    (`((,_ . ,_) nil) nil)
    (`((,kf . ,kr) (,vf . ,vr))
      (glof:conj (glof:zipmap kf vf)
              (glof:zipmap
               kr vr)))
    (`(,k ,v)
      (glof:assoc () k v))))

(cl-defun glof:merge (&rest plists)
  (pcase plists
    (`(,p) p)
    (`(() ()) ())
    (`(() ,p) p)
    (`(,p ()) p)
    (`(,a ,b)
      (apply #'glof:assoc a b))
    (`(,a ,b . ,r)
      (apply #'glof:merge (glof:merge a b) r))))

(cl-defun glof:keyify (&rest names)
  (declare (pure t))
  (pcase names
    (`(,ns ,thing)
      (intern (seq-concatenate 'string
                               ":"
                               (glof:stringify ns)
                               "/"
                               (glof:stringify thing))))
    
    (`(,(and (app type-of `symbol)
             (pred keywordp)
             thing))
      thing)
    (`(,(and (app type-of `symbol)
             thing))
      (glof:keyify (symbol-name thing)))
    ( `(,(and (app type-of `string)
              thing))
       (intern (seq-concatenate 'string
                                ":" thing)))))

(cl-defun glof:stringify (thing)
  (declare (pure t))
  (pcase (type-of thing)
    (`string thing)
    ((and `symbol
          (guard (keywordp thing)))
     (seq-drop (symbol-name thing)
               1))
    (`symbol
     (symbol-name thing))))

(cl-defun glof:empty (&optional _x)
  (declare (pure t))
  ())

(cl-defun glof:eieio-to-plist (object)
  (declare (pure t))
  (cl-letf* ((class (eieio-object-class object))
             (slots (eieio-class-slots class)))
    (seq-mapcat
     (lambda (slot)
       (list
        (glof:keyify slot)
        (eieio-oref  object slot)))
     (seq-map #'eieio-slot-descriptor-name slots))))

(cl-defun glof::seq-empty-p (sequence)
  (and (seqp sequence)
       (seq-empty-p sequence)))

;; TODO
(cl-defun glof:find (p k))

;; TODO
(cl-defmacro glof:let (bindings &body body)
  ())



;; (cl-letf ((plist '(:a 1 :b 2 :c 3)))
;;   (glof:let (((a b c) plist))
;;             `(,a ,b ,c)))

;; ((lambda (binds body)
;;    (if binds
;;        (cl-letf ((bind (car binds)))
;;          `(cl-letf ((,(car (car bind))
;;                       (glof:get ,(cadr bind) ,(glof:keyify (car (car bind))))))
;;             (recur ,(cdr binds))))
;;      ()))
;;  '(((a b c) (:a 1 :b 2 :c 3))))

(provide 'glof)

;;; glof.el ends here

;; Local Variables:
;; nameless-separator: ":"
;; nameless-current-name: "glof"
;; eval: (nameless-mode)
;; End:
