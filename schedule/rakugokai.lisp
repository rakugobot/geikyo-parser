(defpackage #:geikyo-parser/schedule/rakugokai
  (:use #:cl
        #:geikyo-parser/utils
        #:lquery)
  (:import-from #:lquery-funcs
                #:render-text)
  (:import-from #:plump)
  (:import-from #:quri)
  (:export #:parse-rakugokai))
(in-package #:geikyo-parser/schedule/rakugokai)

(defvar *base-uri*
  (quri:uri "http://geikyo.com/schedule/rakugo_detail.php"))

(defun parse-rakugokai (body)
  (let ((table ($1 (initialize body) "#MainCont table")))
    (assert table)
    (loop with rows = ($ table "tr"
                        (combine "th" "td"))
          for i from 0 below (length rows)
          for (th td) = (aref rows i)
          for rowspan = (and (plump:node-p (aref th 0)) (plump:get-attribute (aref th 0) "rowspan"))
          for key = ($1 th (render-text))
          when (stringp key)
          collect (cons key
                        (cond
                          (rowspan
                           (prog1
                               (loop for j from i
                                     repeat (parse-integer rowspan)
                                     for (td td2) = (aref rows j)
                                     for (key val) = (coerce (render-text td2) 'list)
                                     collect (cons key val))
                             (incf i (1- (parse-integer rowspan)))))
                          ((plump:get-elements-by-tag-name (aref td 0) "ul")
                           (coerce
                            (remove-if
                             (lambda (name)
                               (search "仲入り" name))
                             ($ td "ul li" (render-text)))
                            'list))
                          (t
                           ($1 td (render-text))))))))
