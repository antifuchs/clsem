(asdf:defsystem clsem
  :depends-on (cxml-stp drakma closure-html cl-ppcre)
  :components ((:file "package")
               (:file "data" :depends-on ("package"))
               (:file "scraper" :depends-on ("data" "package"))))