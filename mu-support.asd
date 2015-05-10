(asdf:defsystem :mu-support
  :serial t
  :depends-on (:cl-fuseki :hunchentoot :jsown :uuid :local-time)
  :components ((:file "package")
               (:file "helpers")
               (:file "call-support")
               (:file "query-building")
               (:file "repository")))
