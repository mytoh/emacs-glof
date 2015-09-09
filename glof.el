;;; glof -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(cl-defun glof:plist (&rest kvs)
  (apply #'list kvs))

(cl-defun glof:conj (&rest plists)
  (pcase plists
    (`(nil nil) '())
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
  (car p))

(cl-defun glof:val (p)
  (cadr p))

(cl-defun glof:get (p key &optional (default nil))
  (pcase p
    (`() nil)
    (`(,(and k
             (pred (cl-equalp key)))
        ,v)
      v)
    (`(,(and k (guard (not (cl-equalp key k))))
        ,_)
      default)
    (`(,(and k (pred (cl-equalp key)))
        ,v . ,rest)
      v)
    (`(,(and k (guard (not (cl-equalp key k))))
        ,_ . ,rest)
      (glof:get (glof:rest p) key default))))

(cl-defun glof:first (p)
  (pcase-let ((`(,key ,value . ,_)
                p))
    `(,key ,value)))

(cl-defun glof:second (p)
  (glof:first (glof:rest p)))

(cl-defun glof:rest (p)
  (cddr p))

(cl-defun glof:keys (p)
  (pcase p
    (`() '())
    (`(,x . ,_)
      (thread-last p
        glof:rest
        glof:keys
        (cons x)))))

(cl-defun glof:vals (p)
  (pcase p
    (`() '())
    (`(,_ ,x . ,_)
      (thread-last p
        glof:rest
        glof:vals
        (cons x)))))

(cl-defun glof:assoc (plist key value &rest kvs)
  (pcase kvs
    (`()
      (cl-labels ((rec (p flag)
                    (pcase p
                      (`()
                        (if (null flag)
                            (list key value)
                          '()))
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

(cl-defun glof:dissoc (p &rest keys)
  (pcase keys
    (`()
      p)
    (`(nil . ,ks)
      (apply #'glof:dissoc p ks))
    (`(,(and k
             (guard (seq-empty-p p))))
      '())
    (`(,(and k
             (pred (cl-equalp (glof:key (glof:first p))))))
      (glof:rest p))
    (`(,(and k
             (guard (not (cl-equalp k (glof:key (glof:first p)))))))
      (cl-list* (glof:key (glof:first p))
                (glof:val (glof:first p))
                (glof:dissoc (glof:rest p) k)))
    (`(,k nil)
      (glof:dissoc p k))
    (`(,k ,k2 . ,ks)
      (glof:dissoc (glof:dissoc p k)
                   k2 ks))))

(cl-defun glof:alist (p)
  (glof:map
   (pcase-lambda (`(,k ,v)) (cons k v))
   p))

(cl-defun glof:sort-by (p)
  (cl-letf* ((alist (glof:alist p))
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
  (pcase p
    (`() '())
    (_
     (cons (funcall f (glof:first p))
           (glof:map f (glof:rest p))))))

(cl-defun glof:select-keys (p keys)
  (seq-mapcat
   (pcase-lambda ((and k (pred (glof:contains-p p))))
       (list k (glof:get p k)))
   keys))

(cl-defun glof:last (p)
  (pcase p
    (`() '())
    (`(,k ,v) (glof:plist k v))
    (_ (glof:last (glof:rest p)))))

(cl-defun glof:reduce (f p)
  (pcase p
    (`() '())
    (`(,_ ,_) p)
    (_
     (glof:reduce f
                  (glof:conj
                   (funcall f (glof:first p) (glof:second p))
                   (glof:rest (glof:rest p)))))))

(cl-defun glof:entry-p (p)
  (= 2 (seq-length p)))

(cl-defun glof:contains-p (p k)
  (pcase p
    (`() '())
    (`(,(and _
             (pred (cl-equalp k)))
        . ,_)
      t)
    (`(,(and ki
             (guard (not (cl-equalp k ki))))
        ,_ . ,kvr)
      (glof:contains-p kvr k))))

(cl-defun glof:get-in (p ks &optional (default nil))
  (pcase ks
    (`() p)
    (`(,(and k
             (pred (glof:contains-p p))))
      (glof:get p k))
    (`(,(and k
             (guard (not (glof:contains-p p k)))))
      default)
    (`(,(and k
             (pred (glof:contains-p p)))
        . ,kr)
      (glof:get-in (glof:get p k)
                   kr default))
    (`(,(and k
             (guard (not (glof:contains-p p k))))
        . ,_)
      default)))

(cl-defun glof:zipmap (keys vals)
  ;; [[https://www.youtube.com/watch?v=n7aE6k8o_BU]]
  (cl-labels ((rec (plist kys vls)
                (pcase `(,kys ,vls)
                  (`(nil nil) plist)
                  (`(nil (,_ . ,_)) plist)
                  (`((,_ . ,_) nil) plist)
                  (`(,ks ,vs)
                    (rec (glof:assoc plist (car ks) (car vs))
                         (cdr ks) (cdr vs))))))
    (rec '() keys vals)))

(cl-defun glof:keyify (thing)
  (pcase (type-of thing)
    ((and 'symbol
          (guard (keywordp thing)))
     thing)
    (`symbol
     (glof:keyify (symbol-name thing)))
    ((and `string
          type)
     (intern (seq-concatenate 'string
                              ":" thing)))))

(cl-defun glof:stringify (thing)
  (pcase (type-of thing)
    (`string thing)
    ((and `symbol
          (guard (keywordp thing)))
     (seq-drop (symbol-name thing)
               1))
    (`symbol
     (glof:stringify
      (glof:keyify thing)))))

(provide 'glof)

;;; glof.el ends here
