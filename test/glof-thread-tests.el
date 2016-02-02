;;; glof-thread-tests --- glof-thread-tests -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(when (featurep 'glof-thread)
  (unload-feature 'glof-thread 'force))
(require 'ert)
(require 'glof-thread)

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

(ert-deftest glof-test-thread-first ()
  (glof-test-helper-are
   (glof:-> '(:a 1 :b [2 (:c 3)])
            :b 1 :c 1+)
   4))

;;; glof-thread-tests.el ends here
