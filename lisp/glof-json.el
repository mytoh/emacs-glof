;;; glof-json --- glof-json -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl)
(require 'seq)
(require 'glof)

(cl-defun glof-json:parse (json-alist)
  (if json-alist
      (cl-letf ((elm (car json-alist)))
        (glof:conj
         (cl-letf ((key (car elm))
                   (value (cdr elm)))
           (pcase value
             ((pred  vectorp)
              (glof:assoc ()
                          (glof:keyify key)
                          (seq-into
                           (seq-map
                            #'glof-json:parse
                            value)
                           'vector)))
             ((pred listp)
              (glof:assoc ()
                          (glof:keyify key)
                          (glof-json:parse value)))
             (_
              (glof:assoc ()
                          (glof:keyify key)
                          value))))
         (glof-json:parse (cdr json-alist))))
    nil))

(thread-first (pcst-api-init :host "peca2.koti")
  (pcst-api-request "getChannels" )
  glof-json:parse
  )




;;; glof-json.el ends here
