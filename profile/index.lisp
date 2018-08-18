(defpackage #:geikyo-parser/profile/index
  (:use #:cl
        #:geikyo-parser/utils
        #:lquery)
  (:import-from #:quri)
  (:export #:parse-search-results))
(in-package #:geikyo-parser/profile/index)

(defvar *base-uri*
  (quri:uri "http://geikyo.com/profile/"))

(defun parse-search-results (body)
  (let ((rows ($ (initialize body)
                "#MainCont table tr")))
    (assert (< 1 (length rows)))
    (let* ((header (aref rows 0))
           (keys ($ header "th" (render-text))))
      (loop for i from 1 below (length rows)
            for row = (aref rows i)
            for data = ($ row "td" (render-text))
            collect `(("uri" . ,(merge-uris ($1 row "td:nth-child(2) a" (attr "href")) *base-uri*))
                      ,@(loop for key across keys
                              for data across data
                              collect (cons key data)))))))
