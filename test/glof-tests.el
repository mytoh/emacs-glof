;;; glof-tests -*- lexical-binding: t; coding: utf-8; -*-

;;; Code:

(when (featurep 'glof)
  (unload-feature 'glof 'force))
(require 'ert)
(require 'glof)

(ert-delete-all-tests)

(cl-defmacro glof-test-helper-is (expected actual &optional (tester 'cl-equalp))
  `(should (funcall #',tester ,expected ,actual)))

(cl-defmacro glof-test-helper-are (&rest tests)
  (and tests
       `(progn
          (glof-test-helper-is ,(car tests) ,(cadr tests))
          (glof-test-helper-are ,@(cddr tests)))))

(cl-defmacro glof-test-helper-are-form (vars form &rest tests)
  (cl-letf ((lvars (length vars)))
    (and tests
         `(progn
            (should ((lambda ,vars ,form) ,@(seq-take tests lvars)))
            (glof-test-helper-are ,vars ,form ,@(seq-drop tests lvars))))))

;;  prefer (should (eql EXPECTED ACTUAL))

(ert-deftest glof-tests-conj ()
  ;;; github.com/clojure/clojure/test/clojure/test_clojure/data_structures.clj without vector support
  (glof-test-helper-are
   (glof:conj () ()) ()

   (glof:conj () '(:a 1)) '(:a 1)

   (glof:conj () '(:a 1 :b 2)) '(:a 1 :b 2)

   (glof:sort-by (glof:conj () '(:a 1 :b 2) '(:c 3)))
   '(:a 1 :b 2 :c 3)

   (glof:sort-by (glof:conj () '(:a 1 :b 2) '(:a 3 :c 4)))
   '(:a 3 :b 2 :c 4)

   (glof:conj '(:a 1) '(:a 7)) '(:a 7)

   (glof:conj '(:a 1) '(:b 2)) '(:a 1 :b 2)

   (glof:conj '(:a 1) '(:a 7 :b 2)) '(:a 7 :b 2)

   (glof:sort-by (glof:conj '(:a 1) '(:a 7 :b 2) '(:c 3)))
   '(:a 7 :b 2 :c 3)

   (glof:sort-by (glof:conj '(:a 1) '(:a 7 :b 2) '(:b 4 :c 5)))
   '(:a 7 :b 4 :c 5)

   (glof:conj () (glof:first '(:a 1)))
   '(:a 1)

   (glof:conj '(:a 1) (glof:first '(:b 2)))
   '(:a 1 :b 2)

   (glof:conj '(:a 1) (glof:first '(:a 7)))
   '(:a 7)

   (glof:sort-by (glof:conj '(:a 1) (glof:first '(:b 2)) (glof:first '(:a 5))))
   '(:a 5 :b 2)

   ;; nil list check (not necessary?)
   (glof:conj () '(nil ()))
   '(nil nil)

   (glof:conj () '(() nil))
   '(() nil)

   (glof:conj () '(() ()))
   '(() ())
   ))


(ert-deftest glof-tests-sort-by ()
  (glof-test-helper-are
   (glof:sort-by '(:b 2 :a 1))
   '(:a 1 :b 2)
   (glof:sort-by '(:b 2 :c 3 :a 1))
   '(:a 1 :b 2 :c 3)))

;; (ert-deftest glof-tests-sort ()
;;   (glof-test-helper-are
;;    (glof:sort '(:b 2 :a 1))
;;    '(:a 1 :b 2)
;;    (glof:sort '(:b 2 :c 3 :a 1))
;;    '(:a 1 :b 2 :c 3)))

(ert-deftest glof-tests-keys ()
  (glof-test-helper-are
   (glof:names '(:a 1 :b 2)) '(:a :b)))

(ert-deftest glof-tests ()
  (cl-letf ((m '(:a 1 :b 2)))
    (glof-test-helper-are
     (glof:first m) '(:a 1)
     (glof:second m) '(:b 2)
     (glof:values m) '(1 2)
     (glof:map
      (pcase-lambda (`(,k ,v))
          (list k (1+ v)))
      m)
     '(:a 2 :b 3))))

(ert-deftest glof-tests-get ()
  (cl-letf ((p '(:a 1 :b 2 :c ( :d 3 :e 4) :f nil :g false nil ( :h 5))))
    (glof-test-helper-are
     (glof:get p :a) 1
     (glof:get p :e) nil
     (glof:get p :e 0) 0
     (glof:get p nil) '(:h 5)
     (glof:get p :b 0) 2
     (glof:get p :f 0) nil
     (glof:get () :a) nil
     (glof:get () nil) nil
     (glof:get [0 1 2] 1) 1)))

(ert-deftest glof-tests-assoc ()
  (cl-letf ((m '(:a 1 :b 2)))
    (glof-test-helper-are
     (glof:assoc m :b 99)
     '(:a 1 :b 99)
     (glof:assoc m :c 3)
     '(:a 1 :b 2 :c 3)))

  (glof-test-helper-are
   (glof:assoc () :a 1)
   '(:a 1)

   (glof:assoc '(:a 1) :a 3)
   '(:a 3)
   (glof:assoc '(:a 1 :b 2) :b 3)
   '(:a 1 :b 3)

   (glof:assoc () nil 1)
   '(nil 1)

   (glof:sort-by (glof:assoc () :b -2 :a 2))
   '(:a 2 :b -2)

   (glof:sort-by
    (glof:assoc () :b -2 :a 2 :c 3))
   '(:a 2 :b -2 :c 3)))

(ert-deftest glof-tests-dissoc ()
  (glof-test-helper-are
   (glof:dissoc '( :a 1 :b 2 :c 3)) ; dissoc nothing
   '( :a 1 :b 2 :c 3)
   (glof:dissoc '( :a 1 :b 2 :c 3) :b) ; dissoc key :b
   '( :a 1 :c 3)
   (glof:dissoc '( :a 1 :b 2 :c 3) :d) ; dissoc not existed key
   '( :a 1 :b 2 :c 3)
   (glof:dissoc '( :a 1 :b 2 :c 3) :c :b) ; several keys at once
   '( :a 1)
   (glof:dissoc '( :a 1 :b 2 :c 3) () :c :b) ; several keys at once
   '( :a 1)))

(ert-deftest glof-tests-select-keys ()
  (glof-test-helper-are
   (glof:select-keys '(:a 1 :b 2) ())
   ()
   (glof:select-keys '(:a 1 :b 2) [])
   ()
   (glof:select-keys '(:a 1 :b 2) '(:a))
   '(:a 1)
   (glof:select-keys '(:a 1 :b 2) '[:a])
   '(:a 1)
   (glof:select-keys '( :a 1 :b 2 :c 3) '( :a :c))
   '(:a 1 :c 3)
   (glof:select-keys '( :a 1 :b 2 :c 3) '[:a :c])
   '(:a 1 :c 3)
   (glof:select-keys '( :a 1 :b 2 :c 3) '[:a :z])
   '(:a 1)
   (glof:select-keys '( :a 1 :b 2 :c 3) '[:x :y])
   ()
   ))

(ert-deftest glof-tests-alistify ()
  (glof-test-helper-are
   (glof:alistify '(:a 1 :b 2 :c 3))
   '((:a . 1) (:b . 2) (:c . 3))))

(ert-deftest glof-tests-last ()
  (glof-test-helper-are
   (glof:last (glof:plist :a 1 :b 2 :c 3))
   '(:c 3)
   (glof:last (glof:plist))
   ()))

(ert-deftest glof-tests-foldl ()
  (glof-test-helper-are
   (glof:foldl
    (pcase-lambda (`(_ ,a) `(_ ,b))
        (glof:plist :result (+ a b)))
    '(:result 0)
    '(:a 1 :b 2 :c 3))
   '(:result 6)))

(ert-deftest glof-tests-foldr ()
  (glof-test-helper-are
   (glof:foldr
    (pcase-lambda (`(_ ,a) `(_ ,b))
        (glof:plist :result (+ a b)))
    '(:result 0)
    '(:a 1 :b 2 :c 3))
   '(:result 6)))

(ert-deftest glof-tests-unfold ()
  (cl-letf ((plist (glof:plist :a 1 :b 2 :c 3)))
    (glof-test-helper-are
     (glof:unfold
      #'null
      (pcase-lambda (`(,k ,v . ,_))
          (glof:plist
           k (* v v)))
      #'glof:rest
      plist)
     (glof:map
      (pcase-lambda (`(,k ,v))
          (glof:plist
           k (* v v)))
      plist))))

(ert-deftest glof-tests-contains-p ()

  ;;; [[http://github.com/clojure/clojure/test/clojure/test_clojure/data_structures.clj]]
  ;; without vector and set support
  (glof-test-helper-are
   (glof:contains-p () :a) nil
   (glof:contains-p () nil) nil

   (glof:contains-p '(:a 1) :a) t
   (glof:contains-p '(:a 1) :b) nil
   (glof:contains-p '(:a 1) nil) nil
   (glof:contains-p '(nil 1) nil) t

   (glof:contains-p '(:a 1 :b 2) :a) t
   (glof:contains-p '(:a 1 :b 2) :b) t
   (glof:contains-p '(:a 1 :b 2) :c) nil
   (glof:contains-p '(:a 1 :b 2) nil) nil))

(ert-deftest glof-tests-get-in ()

  (cl-letf ((p '(:a 1 :b 2 :c ( :d 3 :e 4) :f nil :g falsey nil ( :h 5)))
            (p2 '(:a 1 :b [(:c 3)] :e [(:f (:g [1 2 3]))])))
    (glof-test-helper-are
  ;;; [[http://github.com/clojure/clojure/test/clojure/test_clojure/data_structures.clj]]
     (glof:get-in p '(:c :e)) 4
     (glof:get-in p '(:c :x)) nil
     (glof:get-in p '(:f)) nil
     (glof:get-in p '(:g)) 'falsey
     (glof:get-in p '(:h)) nil
     (glof:get-in p ()) p
     (glof:get-in p nil) p

     ;; with optional default
     (glof:get-in p '(:c :e) 0) 4
     (glof:get-in p '(:c :x) 0) 0
     (glof:get-in p '(:b) 0) 2
     (glof:get-in p '(:f) 0) nil
     (glof:get-in p '(:g) 0) 'falsey
     (glof:get-in p '(:h) 0) 0
     (glof:get-in p '(:x :y) '( :y 1)) '( :y 1)
     (glof:get-in p () 0) p

     ;; nested vector
     (glof:get-in p2 '(:b 0 :c)) 3
     (glof:get-in p2 '(:d)) nil
     (glof:get-in p2 '(:d) :not-found) :not-found
     (glof:get-in p2 '(:e 0 :f :g 2)) 3
     )))

(ert-deftest glof-tests-get-in-vector ()
  (cl-letf ((p '(:a 1 :b 2 :c ( :d 3 :e 4) :f nil :g falsey nil ( :h 5))))
    (glof-test-helper-are
     (glof:get-in p [:c :e]) 4
     (glof:get-in p [:c :x]) nil
     (glof:get-in p [:f]) nil
     (glof:get-in p [:g]) 'falsey
     (glof:get-in p [:h]) nil
     (glof:get-in p []) p

     ;; with optional default
     (glof:get-in p [:c :e] 0) 4
     (glof:get-in p [:c :x] 0) 0
     (glof:get-in p [:b] 0) 2
     (glof:get-in p [:f] 0) nil
     (glof:get-in p [:g] 0) 'falsey
     (glof:get-in p [:h] 0) 0
     (glof:get-in p [:x :y] '( :y 1)) '( :y 1)
     (glof:get-in p [] 0) p
     )))

(ert-deftest glof-tests-zipmap ()
  (glof-test-helper-are
   (glof:zipmap '(:a :b :c) '(1 2 3)) (glof:plist :a 1 :b 2 :c 3)
   (glof:zipmap '(:a :b :c) '(1 2)) (glof:plist :a 1 :b 2)
   (glof:zipmap '(:a :b :c :d) '(1)) (glof:plist :a 1)
   (glof:zipmap '(:a) '(1 2 3 4)) (glof:plist :a 1)
   (glof:zipmap '(:a :b) '((1) (2))) (glof:plist :a '(1) :b '(2))
   (glof:zipmap () ()) nil
   (glof:zipmap '(:a) ()) nil
   (glof:zipmap '(:a :b) ()) nil
   (glof:zipmap () '(:a)) nil
   (glof:zipmap () '(:a :b)) nil))

(ert-deftest glof-tests-keyword ()
  (cl-letf ((expected :test))
    (glof-test-helper-are
     (glof:keyword :test) expected
     (glof:keyword 'test) expected
     (glof:keyword "test") expected))
  ;; namespace
  (cl-letf ((expected :user/test))
    (glof-test-helper-are
     (glof:keyword "user" "test") expected
     (glof:keyword "user" 'test) expected)))

(ert-deftest glof-tests-string ()
  (cl-letf ((expected "test"))
    (glof-test-helper-are
     (glof:string :test) expected
     (glof:string 'test) expected
     (glof:string "test") expected)))

(ert-deftest glof-tests-update ()
  (glof-test-helper-are
   (glof:update '(:a 1 :b 2) :b #'1+) '(:a 1 :b 3)))

(ert-deftest glof-tests-assoc-in ()
  (glof-test-helper-are
   (glof:assoc-in '(:a (:b (:c 3) :d 4))
                  '(:a :b :c) 99)
   '(:a (:b (:c 99) :d 4))))


(ert-deftest glof-tests-assoc-in-vector ()
  (glof-test-helper-are
   (glof:assoc-in '(:a (:b (:c 3) :d 4))
                  [:a :b :c] 99)
   '(:a (:b (:c 99) :d 4))))

(ert-deftest glof-tests-merge ()
  (glof-test-helper-are
   (glof:merge ())
   ()
   (glof:merge () ())
   ()
   (glof:merge (glof:plist :a 1 :b 2)
               ())
   (glof:plist :a 1 :b 2)
   (glof:merge (glof:plist :a 1 :b 2)
               (glof:plist :c 3))
   (glof:plist :a 1 :b 2 :c 3)
   (glof:merge (glof:plist :a 1 :b 2)
               (glof:plist :c 3)
               (glof:plist :d 4))
   (glof:plist :a 1 :b 2 :c 3 :d 4)
   ))

(ert-deftest glof-tests-find ()
  (glof-test-helper-are
   (glof:find '(:a 1 :b 2 :c 3) :a) '(:a 1)
   (glof:find '(:a 1 :b 2 :c 3) :d) ()
   (glof:find () :d)))

(ert-deftest glof-tests-map-names ()
  (glof-test-helper-are
   (glof:map-names (lambda (x) (* x 2)) '(:a 1 :b 2 :c 3))
   '(:a 2 :b 4 :c 6)))

;;; glof-tests.el ends here
