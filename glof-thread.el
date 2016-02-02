;;; glof-thread --- glof-thread -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'glof)


(cl-defun glof::thread-transform (forms)
  (colle:map
   (lambda (form)
     (pcase form
       (`(,f . ,args)
         form)
       ((or (pred keywordp)
            (pred stringp))
        `(glof:get ,form))
       ((pred vectorp)
        `(glof:get-in ,form))
       (_ form)))
   forms))

(cl-defmacro glof:-> (expr &rest forms)
  `(thread-first ,expr
     ,@(glof::thread-transform forms)))

(cl-defmacro glof:->> (expr &rets forms)
  `(thread-last ,expr
     ,@(glof::thread-transform forms)))

;;; glof-thread.el ends here
