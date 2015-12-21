(asdf:defsystem :mu-support
  :serial t
  :depends-on (:cl-fuseki :hunchentoot :jsown :cl-mongo-id)
  :components ((:file "package")
               (:file "helpers")
               (:file "call-support")
               (:file "query-building")
               (:file "repository")))
