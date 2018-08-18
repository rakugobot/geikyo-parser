(defpackage #:geikyo-parser/utils
  (:use #:cl)
  (:import-from #:quri)
  (:export #:merge-uris))
(in-package #:geikyo-parser/utils)

(defun merge-uris (href base-uri)
  (quri:render-uri
   (quri:merge-uris (quri:uri href) (quri:uri base-uri))))
