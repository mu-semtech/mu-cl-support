(in-package :mu-support)

;;; repository definition
(defun server-location ()
  (if (find :docker *features*)
      (progn (format t "~&running inside a docker~%")
         "http://database:8890")
      (progn (format t "~&running on localhost~%")
         "http://localhost:8890")))

(defparameter *repository*
  (make-instance 'fuseki::virtuoso-repository :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url (server-location))))

(add-prefix "app" "http://mu.semte.ch/app/")
(add-prefix "mu" "http://mu.semte.ch/vocabularies/")
(add-prefix "rm" "http://mu.semte.ch/vocabularies/logical-delete/")

