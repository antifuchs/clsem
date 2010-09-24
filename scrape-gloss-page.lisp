(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cxml-stp)
  (ql:quickload :drakma)
  (ql:quickload :closure-html))

;; "http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_e.htm"

(defvar *xhtml* "http://www.w3.org/1999/xhtml")

(defun scrape-glossary-page (url)
  (let ((document (chtml:parse
                   (drakma:http-request url)
                   (cxml-stp:make-builder))))
    (format t "@base <~A>.~%" url)
    (let ((hr-count 2)
          current-entry)
      (length
       (stp:filter-recursively
        (lambda (node)
          (when (and (typep node 'stp:element)
                     (string-equal "hr" (stp:local-name node)))
            (decf hr-count))
          (when (and (>  hr-count 0)
                     (typep node 'stp:element)
                     (string-equal "a" (stp:local-name node)))
            (cond ((stp:attribute-value node "name")
                   (setf current-entry (stp:attribute-value node "name"))
                   (format t "<#~a> :name \"~a\".~%"
                           current-entry
                           (stp:string-value (first (stp:filter-recursively (stp:of-name "b" *xhtml*)
                                                                            node))))
                   t)
                  ((and current-entry
                        (stp:attribute-value node "href"))
                   (format t "<#~a> :linksTo <~a>.~%"
                           current-entry
                           (stp:attribute-value node "href"))
                   t))))
        document)))))

(defun write-prefixes ()
  (format t "@prefix : <http://boinkor.net/ns/clhs-semantics/>.~%"))

(defun do-it (output-file)
  (with-open-file (*standard-output* output-file :direction :output
                                     :if-exists :supersede :if-does-not-exist :create)
    (write-prefixes)
    (scrape-glossary-page "http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_9.htm")
    (dotimes (i 25)    ; there is no glossary for #\z, so just skip it.
      (scrape-glossary-page
       (format nil "http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_~c.htm"
               (code-char (+ i (char-code #\a))))))))