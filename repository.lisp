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
  (list "mu-session-id" "mu-call-id" "mu-auth-allowed-groups")
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

(defmethod fuseki::query-raw ((repos mu-semtech-repository) (query string) &rest options &key &allow-other-keys)
  (cl-fuseki::flush-updates repos)
  (let ((full-query (apply #'cl-fuseki::query-update-prefixes query options)))
    (fuseki::maybe-log-query full-query)
    (send-dex-request (cl-fuseki::query-endpoint repos)
                      :query full-query)))

(defun send-dex-request (url &key (wanted-status-codes '(200)) (query "") &allow-other-keys)
  ;; (cl-fuseki::remove-key html-args :wanted-status-codes)
  (multiple-value-bind (response status-code response-headers)
      (let ((url (quri:uri url))
            (headers `(("accept" . "application/sparql-results+json")
                       ,@(alexandria:when-let ((header (hunchentoot:header-in* :mu-session-id)))
                           `(("mu-session-id" . ,header)))
                       ,@(alexandria:when-let ((header (hunchentoot:header-in* :mu-call-id)))
                           `(("mu-call-id" . ,header)))
                       ,@(alexandria:when-let
                             ((allowed-groups (or (hunchentoot:header-out :mu-auth-allowed-groups)
                                                  (hunchentoot:header-in* :mu-auth-allowed-groups))))
                           (list (cons "mu-auth-allowed-groups" allowed-groups))))))
        (if (< (length query) 5000) ;; guessing on 5k here
            (progn
              (setf (quri:uri-query-params url)
                    `(("query" . ,query)))
              (dex:request url
                           :method :get
                           :use-connection-pool t
                           :keep-alive t
                           :force-string t
                           ;; :verbose t
                           :headers headers))
            (dex:request url
                         :method :post
                         :use-connection-pool nil
                         :keep-alive nil
                         :force-string t
                         :headers headers
                         :content `(("query" . ,query)))))
    (unless (and wanted-status-codes
                 (find status-code wanted-status-codes))
      (error 'cl-fuseki:sesame-exception
             :status-code status-code
             :response response))

    ;; make sure the authorization keys are set on the response
    (alexandria:when-let ((received-auth-allowed-groups
                           (gethash "mu-auth-allowed-groups" response-headers)))
      (setf (hunchentoot:header-out :mu-auth-allowed-groups)
            received-auth-allowed-groups))
    (alexandria:when-let ((received-auth-used-groups
                           (gethash "mu-auth-used-groups" response-headers)))
      ;; TODO: this second one is incorrect, we should actually join
      ;; the used groups.  this will take a bit more time and the
      ;; joining is not necessary yet.
      (setf (hunchentoot:header-out :mu-auth-used-groups)
            received-auth-used-groups))
    response))

(defmethod fuseki::update-now ((repos mu-semtech-repository) (update string))
  (fuseki::maybe-log-query update)
  (send-sparql-request (fuseki::update-endpoint repos)
                       :wanted-status-codes '(200 204) ; only 204 is in the spec
                       :method :post
                       :parameters `(("query" . ,update))
                       :additional-headers (mu-semtech-passed-headers)))


(defun send-sparql-request (url &rest html-args &key (wanted-status-codes '(200)) &allow-other-keys)
  (cl-fuseki::remove-key html-args :wanted-status-codes)
  (multiple-value-bind (response status-code response-headers)
      (apply #'drakma:http-request url :force-binary t html-args)
    (unless (and wanted-status-codes
                 (find status-code wanted-status-codes))
      (error 'cl-fuseki:sesame-exception
             :status-code status-code
             :response response))

    ;; make sure the authorization keys are set on the response
    (alexandria:when-let ((received-auth-allowed-groups
                (cdr (assoc :mu-auth-allowed-groups response-headers))))
      (setf (hunchentoot:header-out :mu-auth-allowed-groups)
            received-auth-allowed-groups))
    (alexandria:when-let ((received-auth-used-groups
                (cdr (assoc :mu-auth-used-groups response-headers))))
      ;; TODO: this second one is incorrect, we should actually join
      ;; the used groups.  this will take a bit more time and the
      ;; joining is not necessary yet.
      (setf (hunchentoot:header-out :mu-auth-used-groups)
            received-auth-used-groups))
    (let ((result (flexi-streams:octets-to-string response :external-format :utf-8)))
      result)))


(defparameter *repository*
  (make-instance 'mu-semtech-repository :name "main repository"
                 :server (make-instance 'fuseki::virtuoso-server
                                        :base-url (server-location))))

(add-prefix "app" "http://mu.semte.ch/app/")
(add-prefix "xsd" "http://www.w3.org/2001/XMLSchema#")
(add-prefix "mu" "http://mu.semte.ch/vocabularies/core/")
(add-prefix "typedLiterals" "http://mu.semte.ch/vocabularies/typed-literals/")
(add-prefix "rm" "http://mu.semte.ch/vocabularies/logical-delete/")

