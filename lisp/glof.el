;;; glof -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'pcase)

(require 'colle)

(require 'glof-lookup)
(require 'glof-let)
(require 'glof-null)
(require 'glof-size)
(require 'glof-member)

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
                               x (glof:name (glof:first p)))))
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


(cl-defun glof:name (p)
  (declare (pure t))
  (pcase p
    (`(,n ,_)
      n)))

(cl-defun glof:value (p)
  (declare (pure t))
  (pcase p
    (`(,_ ,v)
      v)))

(cl-defun glof:get (p key &optional (default nil))
  (declare (pure t))
  (pcase p
    (`()
      (pcase default
        (`nil nil)
        (_ default)))
    (`(,(pred (cl-equalp key))
        ,v)
      v)
    ((and (pred vectorp)
        (guard (numberp key)))
     (seq-elt p key))
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

(cl-defun glof:names (p)
  (declare (pure t))
  (glof:foldr
   (pcase-lambda (`(,n ,v) a)
       (cons n a))
   ()
   p))

(cl-defun glof:values (p)
  (declare (pure t))
  (glof:foldr
   (pcase-lambda (`(,n ,v) a)
       (cons v a))
   ()
   p))

(cl-defun glof:assoc (plist key value &rest kvs)
  (declare (pure t))
  (pcase kvs
    (`()
      (cl-labels ((rec (p flag)
                    (pcase p
                      (`()
                        (if (null flag)
                            (glof:prop key value)
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
    ((pred colle:empty-p) p)
    ((seq k
          (pred colle:empty-p))
     
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
    (`(,(pred (cl-equalp (glof:name (glof:first p)))))
      (glof:rest p))
    (`(,(and k
           (guard (not (cl-equalp k (glof:name (glof:first p)))))))
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
  (seq-reverse
   (glof:foldl
    (pcase-lambda (r `(,k ,v))
        (cons (cons k v) r))
    (glof:empty)
    p)))

(cl-defun glof:sort-by (p pred)
  (declare (pure t))
  (cl-letf* ((alist (glof:alistify p))
             (sorted
              (cl-sort alist
                       (pcase-lambda (`(,a . ,_) `(,b . ,_))
                           (funcall pred a b)))))
    (seq-mapcat
     (pcase-lambda (`(,k . ,v)) (glof:prop k v))
     sorted)))

(cl-defun glof:sort (p)
  (glof:sort-by p #'string<))

;; [[http://www.cs.toronto.edu/~dianaz/Example_LispPart1.html]]
;; (defun glof:sort (p)
;;   (pcase p
;;     (`() '())
;;     (`(,_ ,_) p)
;;     ((guard (string< (glof:name (glof:first p))
;;                      (glof:name (glof:second p))))
;;      (glof::check-again (glof:conj (glof:first p)
;;                                      (glof:sort (glof:rest p)))))
;;     (_
;;      (glof::check-again
;;       (glof:conj (glof:second p)
;;                   (glof:sort (glof:conj (glof:first p)
;;                                           (glof:rest (glof:rest p)))))))))

;; (defun glof::check-again (p)
;;   (if (string< (glof:name (glof:first p))
;;                (glof:name (glof:second p)))
;;       p
;;     (glof:sort p)))

(cl-defun glof:map (f p)
  (glof:foldr
   (lambda (a l)
     (glof:conj (funcall f a) l))
   (glof:empty)
   p))

(cl-defun glof:select-keys (p keys)
  (declare (pure t))
  (seq-mapcat
   (pcase-lambda ((and k (pred (glof:contains-p p))))
       (pcase (glof:get p k)
         (`() nil)
         (found (glof:prop k found))))
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
    ((and (pred seqp)
        (pred colle:empty-p))
     p)
    ((and (guard (vectorp p))
        (seq (and k
                (pred numberp))
             (pred colle:empty-p)))
     (glof:get p k default))
    ((seq k (pred colle:empty-p))
     (glof:get p k default))
    ((and (guard (consp p))
        (seq (and k
                (guard (not (glof:contains-p p k))))))
     default)
    ((and (guard (vectorp p))
        (seq (and k
                (pred numberp))
             &rest kr))
     (glof:get-in (glof:get p k) kr default))
    ((seq k &rest kr)
     (glof:get-in (glof:get p k)
              kr default))))

(cl-defun glof:update (p k f)
  (declare (pure t))
  (glof:assoc p k
          (funcall f (glof:get p k))))

(cl-defun glof:zipmap (keys vals)
  ;; [[https://www.youtube.com/watch?v=n7aE6k8o_BU]]
  (declare (pure t))
  (pcase `(,keys ,vals)
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

(cl-defun glof:keyword (&rest names)
  (declare (pure t))
  (pcase names
    (`(,ns ,thing)
      (intern (seq-concatenate 'string
                               ":"
                               (glof:string ns)
                               "/"
                               (glof:string thing))))
    
    (`(,(and (app type-of `symbol)
           (pred keywordp)
           thing))
      thing)
    (`(,(and (app type-of `symbol)
           thing))
      (glof:keyword (symbol-name thing)))
    ( `(,(and (app type-of `string)
            thing))
       (intern (seq-concatenate 'string
                                ":" thing)))))

(cl-defun glof:string (thing)
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

(cl-defun glof:eieio->plist (object)
  (declare (pure t))
  (cl-letf* ((class (eieio-object-class object))
             (slots (eieio-class-slots class)))
    (seq-mapcat
     (lambda (slot)
       (glof:prop
        (glof:keyword slot)
        (eieio-oref object slot)))
     (seq-map #'eieio-slot-descriptor-name slots))))

(cl-defun glof::seq-empty-p (sequence)
  (and (seqp sequence)
     (seq-empty-p sequence)))

(cl-defun glof:find (p k)
  (declare (pure t))
  (pcase p
    (`() ())
    (`(,(pred (cl-equalp k)) . ,_)
      (glof:first p))
    (_
     (glof:find (glof:rest p) k))))

(cl-defun glof:prop (n v)
  (list n v))
(defalias 'glof:singleton #'glof:prop)

;; TODO
(cl-defun glof:dissoc-in ())

;; TODO
;; clojure/algo.generic 
(cl-defun glof:map-names (f p)
  (glof:foldr
   (pcase-lambda (`(,k ,v) a)
       (glof:conj (glof:plist k (funcall f v)) a))
   (glof:empty)
   p))


(cl-defun glof:call (p f &rest args)
  (apply #'funcall (glof:get p f) args))

(cl-defmacro glof:do ((n v plist) &body body)
  (cl-letf ((p (gensym "glof-do-")))
    `(cl-letf ((,p ,plist))
       (while ,p
         (cl-letf ((,n (car ,p))
                   (,v (cadr ,p)))
           ,@body)
         (setq ,p (cddr ,p))))))


(cl-defun glof:ns (ns p)
  (glof:foldr
   (lambda (e r)
     (append (list (glof:keyword ns (glof:name e))
                   (glof:value e))
             r))
   ()
   p))

;; (glof:ns 'my '(:a 1 ::b 2))


;; (cl-letf ((plist '(:a 1 :b 2 :c 3)))
;;   (glof:let (((a b c) plist))
;;             `(,a ,b ,c)))

;; ((lambda (binds body)
;;    (if binds
;;        (cl-letf ((bind (car binds)))
;;          `(cl-letf ((,(car (car bind))
;;                       (glof:get ,(cadr bind) ,(glof:keyword (car (car bind))))))
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
