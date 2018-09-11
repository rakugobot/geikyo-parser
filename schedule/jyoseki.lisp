(defpackage #:geikyo-parser/schedule/jyoseki
  (:use #:cl
        #:geikyo-parser/utils
        #:rakugobot-utils
        #:lquery)
  (:import-from #:lquery-funcs
                #:render-text)
  (:import-from #:quri)
  (:import-from #:plump)
  (:import-from #:assoc-utils
                #:aget)
  (:export #:parse-jyoseki))
(in-package #:geikyo-parser/schedule/jyoseki)

(defvar *base-uri*
  (quri:uri "http://geikyo.com/schedule/jyoseki_detail.php"))

(defun %parse-header (main)
  (let* ((header (or ($1 main "#JyosekTtl")
                     (error "#JyosekTtl is missing")))
         (day ($1 header ".day"))
         (hall ($1 header "a" (text)))
         (default-month (nth-value 4 (decode-universal-time (get-universal-time)))))
    (ppcre:register-groups-bind (title (#'parse-integer month start-day end-day))
        ("^((?:..?)月(?:..?)席.*)\\s*(?:(\\d{1,2})月)?(\\d{1,2})(?:〜(\\d{1,2}))?日"
         (plump:text day))
      `(("venue" . ,hall)
        ("title" . ,(format nil "~A ~A" hall title))
        ("date-from" . ,(format nil "~D-~2,'0D-~2,'0D"
                                (get-year-of-month (or month default-month))
                                (or month default-month)
                                start-day))
        ,@(and end-day
               `(("date-to" . ,(format nil "~D-~2,'0D-~2,'0D"
                                       (get-year-of-month (or month default-month))
                                       (or month default-month)
                                       end-day))))))))

(defun %parse-jyoseki-html (body)
  (let* ((main ($1 (initialize body) "#MainCont"))
         (tables ($ main "table")))
    (assert main)
    (assert (< 0 (length tables)))
    (nconc
     (%parse-header main)
     `(("tables" .
        ,(loop for table across tables
               for rows = ($ table "tr")
               when (< 2 (length rows))
               collect
                  (let ((time (render-text (aref rows 1))))
                    (ppcre:register-groups-bind ((#'parse-integer start-hour start-min end-hour end-min))
                        ("(\\d{1,2}):(\\d{1,2})〜(\\d{1,2}):(\\d{1,2})" time)
                      `(("subtitle" . ,($1 rows "th" (render-text)))
                        ("time-from" . ,(format nil "~2,'0D:~2,'0D" start-hour start-min))
                        ("time-to" . ,(format nil "~2,'0D:~2,'0D" end-hour end-min))
                        ("performers" . ,(coerce
                                          (remove nil
                                                  ($ rows "td:nth-child(3)"
                                                    (combine "a" (text))
                                                    (map-apply (lambda (a name)
                                                                 (ppcre:register-groups-bind (name)
                                                                     ("(.+?)[\\s　]+［.+?］" name)
                                                                   (let ((href ($1 a (attr "href"))))
                                                                     `(("name" . ,(substitute #\Space #\　 name))
                                                                       ,@(and href
                                                                              `(("uri" . ,(merge-uris href *base-uri*)))))))))))
                                          'list)))))))))))

(defun parse-jyoseki (body)
  (let ((res (%parse-jyoseki-html body)))
    (loop for table in (aget res "tables")
          collect (let ((hall (normalize-hall-name (aget res "venue"))))
                    `(("title" . ,(format nil "~A ~A" (aget res "title") (aget table "subtitle")))
                      ("start-date" . ,(aget res "date-from"))
                      ("end-date" . ,(aget res "date-to"))
                      ("start-time" . ,(aget table "time-from"))
                      ("end-time" . ,(aget table "time-to"))
                      ("place" . ,hall)
                      ("address" . ,(hall-address hall))
                      ("performers" . ,(aget table "performers")))))))
