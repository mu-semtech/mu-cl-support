(asdf:defsystem :mu-support
  :name "mu-support"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.2.0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Base support for mu.semte.ch microservices written in Common Lisp."
  :serial t
  :depends-on (:cl-fuseki :hunchentoot :jsown :cl-mongo-id)
  :components ((:file "package")
               (:file "helpers")
               (:file "hunchentoot-server")
               (:file "call-support")
               #+java-backend
               (:file "abcl-helpers")
               (:file "query-building")
               (:file "repository")))
