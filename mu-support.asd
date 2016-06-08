(asdf:defsystem :mu-support
  :name "mu-support"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "1.2.0"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Base support for mu.semte.ch microservices written in Common Lisp."
  :serial t
  :depends-on (:cl-fuseki :hunchentoot :jsown :cl-mongo-id
                          #+java-backend :uuid
                          #+java-backend :closer-mop
                          #+java-backend :cl-ppcre
                          #+java-backend :split-sequence
                          #+java-backend :alexandria)
  :components ((:file "package")
               (:file "helpers")
               #-java-backend
               (:file "hunchentoot-server")
               #-java-backend
               (:file "call-support")
               #+java-backend
               (:file "abcl-helpers")
               #+java-backend
               (:file "java-server")
               (:file "query-building")
               (:file "repository")))
