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

(defparameter *mu-semtech-passed-headers*
  (list "mu-session-id" "mu-call-id")
  "List of headers which are read from the request and passed to
   the client application.")

(defun mu-semtech-passed-headers ()
  "Retrieves the headers (as a string) and their contents in a
   way that's easy to pass through to the sparql endpoint."
  (loop for header-string in *mu-semtech-passed-headers*
     for header-key = (intern (string-upcase header-string) :keyword)
     for header-value = (webserver:header-in* header-key)
     if header-value
     collect (cons header-string header-value)))

(defmethod fuseki::query-raw ((repos mu-semtech-repository) (query string)
			      &rest options &key &allow-other-keys)
  (fuseki::flush-updates repos)
  (let ((full-query (apply #'fuseki::query-update-prefixes query options)))
    (fuseki::maybe-log-query full-query)
    (fuseki::send-request (fuseki::query-endpoint repos)
                  :accept (fuseki::get-data-type-binding :json)
                  :parameters `(("query" . ,full-query))
		  :additional-headers (mu-semtech-passed-headers))))

(defparameter *repository*
  (make-instance 'mu-semtech-repository :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url (server-location))))

(add-prefix "app" "http://mu.semte.ch/app/")
(add-prefix "xsd" "http://www.w3.org/2001/XMLSchema#")
(add-prefix "mu" "http://mu.semte.ch/vocabularies/core/")
(add-prefix "typedLiterals" "http://mu.semte.ch/vocabularies/typed-literals/")
(add-prefix "rm" "http://mu.semte.ch/vocabularies/logical-delete/")

