(defpackage #:geikyo-parser/profile/detail
  (:use #:cl
        #:geikyo-parser/utils
        #:lquery)
  (:import-from #:lquery-funcs
                #:render-text
                #:text
                #:attr)
  (:import-from #:quri)
  (:import-from #:str
                #:trim)
  (:export #:parse-profile))
(in-package #:geikyo-parser/profile/detail)


(defvar *base-uri*
  (quri:uri "http://geikyo.com/profile/profile_detail.php"))

(defun %parse-jyoseki-title (link-title)
  (or (ppcre:register-groups-bind (title (#'parse-integer month start-day end-day))
          ("^((?:..?)月(?:..?)席(?:[　)) ]+?(?:前半|後半))?)\\s*(\\d{1,2})月(\\d{1,2})日〜(\\d{1,2})日"
           link-title)
        (let ((year (nth-value 5 (decode-universal-time (get-universal-time)))))
          `(("title" . ,title)
            ("date-from" . ,(format nil "~D-~2,'0D-~2,'0D" year month start-day))
            ("date-to" . ,(format nil "~D-~2,'0D-~2,'0D" year month end-day)))))
      `(("title" . ,link-title))))

(defun %parse-rakugokai-date (date)
  (or (ppcre:register-groups-bind ((#'parse-integer year month day))
          ("(\\d{4})年(\\d{1,2})月(\\d{1,2})日" date)
        `(("date" . ,(format nil "~D-~2,'0D-~2,'0D" year month day))))
      (error "Invalid rakugokai date format: ~S" date)))

(defun parse-profile (body)
  (let* ((main ($ (initialize body) "#MainCont"))
         (profile ($1 main ".Profile"))
         (jyoseki-schedules ($ main "#JyosekiSchedule"))
         (rakugokai-schedules ($ main "#RakugoSchedule")))
    (assert profile)
    (let ((profile
            ($ profile "dl"
              (combine "dt" "dd")
              (map-apply (lambda (dt dd)
                           (cons (aref (render-text dt) 0)
                                 (trim (aref (text dd) 0)))))))
          (jyoseki-schedules ($ jyoseki-schedules "li"
                               (combine "span" "a" "a:nth-child(2)")
                               (map-apply (lambda (span a title)
                                            (let ((title (aref (render-text title) 0)))
                                              (nconc
                                               (%parse-jyoseki-title title)
                                               `(("venue" . ,(aref (render-text span) 0))
                                                 ("uri" . ,(merge-uris ($1 a (attr "href")) *base-uri*)))))))))
          (rakugokai-schedules ($ rakugokai-schedules "li"
                                 (combine "em" "img" "span" "a")
                                 (map-apply (lambda (em img span a)
                                              `(,@(%parse-rakugokai-date (aref (render-text em) 0))
                                                ("title" . ,(render-text (aref span 0)))
                                                ("category" . ,($1 img (attr "alt")))
                                                ("uri" . ,(merge-uris ($1 a (attr "href")) *base-uri*))))))))
      (nconc (coerce profile 'list)
             `(("jyoseki-schedules" . ,(coerce jyoseki-schedules 'list))
               ("rakugokai-schedules" . ,(coerce rakugokai-schedules 'list)))))))
