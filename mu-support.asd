(asdf:defsystem :mu-support
  :name "mu-support"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.2.0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Base support for mu.semte.ch microservices written in Common Lisp."
  :serial t
  :depends-on (:cl-fuseki #-java-backend :hunchentoot :jsown #-java-backend :cl-mongo-id
                          #+java-backend :uuid
                          #+java-backend :cl-fuseki-weblogic-plugin)
  :components ((:file "package")
               (:file "helpers")
               #-java-backend
               (:file "hunchentoot-server")
               #-java-backend
               (:file "call-support")
               #+java-backend
               (:file "java-server")
               (:file "query-building")
               (:file "repository")))
