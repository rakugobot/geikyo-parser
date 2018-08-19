(defpackage #:geikyo-parser/profile/detail
  (:use #:cl
        #:geikyo-parser/utils
        #:lquery)
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
                           (cons ($1 dt (render-text))
                                 (trim ($1 dd (text))))))))
          (jyoseki-schedules ($ jyoseki-schedules "li"
                               (combine "span" "a" "a:nth-child(2)")
                               (map-apply (lambda (span a title)
                                            (let ((title ($1 title (render-text))))
                                              (nconc
                                               (%parse-jyoseki-title title)
                                               `(("venue" . ,($1 span (render-text)))
                                                 ("uri" . ,(merge-uris ($1 a (attr "href")) *base-uri*)))))))))
          (rakugokai-schedules ($ rakugokai-schedules "li"
                                 (combine "em" "img" "span" "a")
                                 (map-apply (lambda (em img span a)
                                              `(,@(%parse-rakugokai-date ($1 em (render-text)))
                                                ("title" . ,($1 span (render-text)))
                                                ("category" . ,($1 img (attr "alt")))
                                                ("uri" . ,(merge-uris ($1 a (attr "href")) *base-uri*))))))))
      (nconc (coerce profile 'list)
             `(("jyoseki-schedules" . ,(coerce jyoseki-schedules 'list))
               ("rakugokai-schedules" . ,(coerce rakugokai-schedules 'list)))))))
