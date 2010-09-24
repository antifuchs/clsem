(asdf:defsystem clsem
  :depends-on (cxml-stp drakma closure-html cl-ppcre)
  :components ((:file "package")
               (:file "data")
               (:file "scraper")))