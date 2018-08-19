(defpackage #:geikyo-parser/utils
  (:use #:cl)
  (:import-from #:quri)
  (:import-from #:local-time)
  (:export #:merge-uris
           #:get-year-of-month))
(in-package #:geikyo-parser/utils)

(defun merge-uris (href base-uri)
  (quri:render-uri
   (quri:merge-uris (quri:uri href) (quri:uri base-uri))))

(defun get-year-of-month (month)
  (let ((today (local-time:today)))
    (if (< month (local-time:timestamp-month today))
        (1+ (local-time:timestamp-year today))
        (local-time:timestamp-year today))))
