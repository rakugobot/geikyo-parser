(uiop:define-package #:geikyo-parser
  (:nicknames #:geikyo-parser/main)
  (:use #:cl)
  (:use-reexport #:geikyo-parser/profile/index
                 #:geikyo-parser/profile/detail
                 #:geikyo-parser/schedule/jyoseki
                 #:geikyo-parser/schedule/rakugokai))
(in-package #:geikyo-parser/main)
