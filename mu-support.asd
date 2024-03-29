(asdf:defsystem :mu-support
  :name "mu-support"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.3.0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Base support for mu.semte.ch microservices written in Common Lisp."
  :serial t
  :depends-on (:cl-fuseki :hunchentoot :jsown :cl-mongo-id :dexador :quri)
  :components ((:file "package")
               (:file "helpers")
               (:file "hunchentoot-server")
               (:file "call-support")
               (:file "query-building")
               (:file "repository")))
