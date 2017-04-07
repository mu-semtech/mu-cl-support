(in-package :mu-support)

;;; repository definition
(defun server-location ()
  (if (find :docker *features*)
      (progn (format t "~&running inside a docker~%")
         "http://database:8890")
      (progn (format t "~&running on localhost~%")
         "http://localhost:8890")))

(defclass mu-semtech-repository (cl-fuseki::virtuoso-repository)
  ()
  (:documentation "repository for mu-semtech Virtuoso endpoints."))

(defun mu-semtech-headers ()
  (list (cons :MU-SESSION-ID (webserver:header-in* :MU-SESSION-ID))))

(defmethod fuseki::query-raw ((repos musemtech-repository) (query string)
			      &rest options &key &allow-other-keys)
  (fuseki::flush-updates repos)
  (let ((full-query (apply #'fuseki::query-update-prefixes query options)))
    (fuseki::maybe-log-query full-query)
    (fuseki::send-request (fuseki::query-endpoint repos)
                  :accept (fuseki::get-data-type-binding :json)
                  :parameters `(("query" . ,full-query))
		  :additional-headers (mu-semtech-headers))))

(defparameter *repository*
  (make-instance 'musemtech-repository :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url (server-location))))

(add-prefix "app" "http://mu.semte.ch/app/")
(add-prefix "xsd" "http://www.w3.org/2001/XMLSchema#")
(add-prefix "mu" "http://mu.semte.ch/vocabularies/core/")
(add-prefix "typedLiterals" "http://mu.semte.ch/vocabularies/typed-literals/")
(add-prefix "rm" "http://mu.semte.ch/vocabularies/logical-delete/")

