(asdf:defsystem :mu-support
  :serial t
  :depends-on (:cl-fuseki :hunchentoot :jsown)
  :components ((:file "package")
               (:file "helpers")
               (:file "call-support")
               (:file "query-building")
               (:file "repository")))
