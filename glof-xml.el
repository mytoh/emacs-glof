;;; glof-xml --- glof-xml -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl)
(require 'seq)
(require 'glof)

(cl-defun glof-xml:parse (html-alist)
  (if html-alist
      (cl-letf ((tag (car html-alist))
                (attrs (cadr html-alist))
                (content (cddr html-alist)))
        (glof:assoc ()
                    :tag (glof:keyify tag)
                    :attrs (glof-xml:handle-attrs attrs)
                    :content
                    (seq-into (glof-xml:handle-content content)
                              'vector)))
    nil))

(cl-defun glof-xml:handle-content (content)
  (if content 
      (seq-map
       (lambda (c)
         (pcase c
           ((pred stringp) c)
           ((pred listp)
            (glof-xml:parse c))))
       content)
    []))

(cl-defun glof-xml:handle-attrs (attrs)
  (if attrs
      (seq-mapcat
       (pcase-lambda (`(,name . ,value))
           `(,(glof:keyify name)
             ,value))
       attrs)
    []))


(cl-defun glof-xml:get (xml query)
  (pcase query
    ((pred numberp)
     (seq-elt xml query))
    (_
     (glof:get xml query))))


(cl-defun glof-xml:xml-> (xml query &rest queries)
  (if xml
      (cl-letf ((res (pcase query
                       ((pred keywordp)
                        (cl-letf ((tag (glof-xml:get xml :tag)))
                          (if tag
                              (glof-xml:get xml :content)
                            nil)))
                       ((pred functionp)
                        (funcall query xml))
                       (_ (glof-xml:get xml query)))))
        (if res
            (if (car queries)
                (apply #'glof-xml:xml-> res
                       (car queries)
                       (cdr queries))
              res)
          nil))
    nil))

(cl-defun glof-xml:attr= (attr value)
  (lambda (xml)
    (pcase xml
      ((pred vectorp)
       (seq-find
        (lambda (x)
          (if (stringp x)
              nil
            (cl-letf ((attrs (glof:get x :attrs)))
              (if (equal value (glof:get attrs attr))
                  x nil))))
        xml))
      ((pred stringp)
       nil)
      (_
       (cl-letf ((attrs (glof:get xml :attrs)))
         (if (equal value (glof:get attrs attr))
             xml nil))))))

(cl-defun glof-xml:attr (attr)
  (lambda (xml)
    (glof:get
     (glof:get xml :attrs)
     attr)))


(provide 'glof-xml)






;;;;;;;;;;;;;;;;;;


;; (glof-xml:xml-> (glof-xml:parse test-xml)
;;                 :top
;;                 0
;;                 :catalog
;;                 0
;;                 :cd
;;                 :)

;; (glof-xml:parse test-xml)

;; (cl-defun test-get-html (where)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        where)
;;     (libxml-parse-html-region
;;      (point-min) (point-max))))

;; (cl-defun test-get-xml-file ()
;;   (with-temp-buffer
;;     (insert-file-contents "test.xml")
;;     (libxml-parse-html-region
;;      (point-min) (point-max))))

;; (test-get-xml-file)

;; (setq test-xml (test-get-html "http://www.xmlfiles.com/examples/cd_catalog.xml"))
;; (setq test-html (test-get-html "http://google.com"))

;; (caddr (seq-elt test-html 2))
;; test-html


;; (thread-first (glof-xml:parse test-html)
;;   (glof-xml:get :content)
;;   (glof-xml:get 0)
;;   (glof-xml:get :content)
;;   (glof-xml:get 11)
;;   (glof-xml:get :content))

;; (thread-first (glof-xml:parse (test-get-xml-file))
;;   (glof-xml:get :content)
;;   (glof-xml:get 0)
;;   (glof-xml:get :content)
;;   )
