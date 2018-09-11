(defpackage #:geikyo-parser/schedule/rakugokai
  (:use #:cl
        #:geikyo-parser/utils
        #:lquery)
  (:import-from #:lquery-funcs
                #:render-text)
  (:import-from #:plump)
  (:import-from #:quri)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-rakugokai))
(in-package #:geikyo-parser/schedule/rakugokai)

(defvar *base-uri*
  (quri:uri "http://geikyo.com/schedule/rakugo_detail.php"))

(defun %parse-rakugokai-html (body)
  (let ((table ($1 (initialize body) "#MainCont table")))
    (assert table)
    (loop with rows = ($ table "tr"
                        (combine "th" "td"))
          for i from 0 below (length rows)
          for (th td) = (aref rows i)
          for rowspan = (and (plump:node-p (aref th 0)) (plump:get-attribute (aref th 0) "rowspan"))
          for key = ($1 th (render-text))
          when (stringp key)
          collect (cond
                    ((string= key "番組（出演者）")
                     (cons key
                           (coerce
                            ($ td "ul" "li"
                              (map (lambda (li)
                                     (let ((a ($1 li "a")))
                                       (if a
                                           `(("name" . ,($1 a (render-text)))
                                             ("uri" . ,(merge-uris ($1 a (attr "href")) *base-uri*)))
                                           `(("name" . ,($1 li (render-text)))))))))
                            'list)))
                    (t (cons key
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
                                ($1 td (render-text))))))))))

(defun parse-rakugokai (body)
  (let ((res (%parse-rakugokai-html body)))
    `(("title" . ,(aget res "公演名"))
      ("start-date" . ,(ppcre:register-groups-bind ((#'parse-integer year month day))
                           ("(\\d{4})年(\\d{1,2})月(\\d{1,2})日" (aget res "開催日"))
                         (format nil "~D-~2,'0D-~2,'0D" year month day)))
      ("start-time" . ,(aget res "開演時間"))
      ("place" . ,(ppcre:regex-replace "\\s*※.+?$" (aget res "会場") ""))
      ("performers" . ,(aget res "番組（出演者）")))))
