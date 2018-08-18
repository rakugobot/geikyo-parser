(defsystem "geikyo-parser"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Parser for an HTML of geikyo.com"
  :depends-on ("geikyo-parser/main")
  :in-order-to ((test-op (test-op "geikyo-parser/tests"))))

(register-system-packages "lquery" '(:lquery :lquery-funcs))
