;;; glof-thread --- glof-thread -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'glof)
(require 'colle)

(cl-defun glof::thread-flipped-get (k p)
  (glof:get p k))

(cl-defun glof::thread-transform (first? forms)
  (colle:map
   (lambda (form)
     (pcase form
       (`(,f . ,args)
         form)
       ((or (pred keywordp)
            (pred stringp)
            (pred numberp))
        (if first?
            `(glof:get ,form)
          `(glof::thread-flipped-get ,form)))
       ((pred vectorp)
        `(glof:get-in ,form))
       (_ form)))
   forms))

(cl-defmacro glof:-> (expr &rest forms)
  `(thread-first ,expr
     ,@(glof::thread-transform t forms)))

(cl-defmacro glof:->> (expr &rest forms)
  `(thread-last ,expr
     ,@(glof::thread-transform nil forms)))

(provide 'glof-thread)

;;; glof-thread.el ends here
