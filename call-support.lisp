(in-package :mu-support)

(defun post-body ()
  "Retrieves the current post-body and parses it as text."
  (hunchentoot:raw-post-data :external-format :utf8 :force-text t))

;;;;;;;;;;;;;;;;;
;;;; call support
;;;;
;;;; Implements support for easily and cleanly defining REST-based
;;;; JSON APIs and helps you host the server.  Most of this is stolen
;;;; from micro-framework.

;;;;;;;;;;;;;;;;;;;;;;;
;;; support for waiting
(defun wait-for-page ()
  "Pages may be commanded to wait for a certain amount of time by
  setting the *average-wait* and *wait-fluctuation* parameters.  This
  function sleeps for that amount of time."
  (declare (special *average-wait* *wait-fluctuation*))
  (when (and (boundp '*average-wait*) (boundp '*wait-fluctuation*)
             (numberp *average-wait*) (numberp *wait-fluctuation*))
    (let ((base-wait-time (- *average-wait* (/ *wait-fluctuation* 2)))
          (fluctuation (random (/ (coerce *wait-fluctuation* 'float) 2))))
      (sleep (+ base-wait-time fluctuation)))))

;;;;;;;;;;;;;;;;;;
;;; defining pages
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun components-to-regex (components &optional (to-end "$"))
    "creates regex from the components of a url.
    to-end is the last portion of the url to be matched.  it defaults
    to \"$\", which means to the end of the line.  substituting it
    with \"\\\\?.*$\" will allow keys to be given near the end of the
    string."
    (format nil "^/*~{~a~^/+~}/*~A"
            (mapcar (lambda (x)
                      (if (keywordp x)
                          (string-downcase (symbol-name x))
                          "([^/]+)"))
                    components)
            to-end)))

(defun create-typed-regex-dispatcher (method regex handler)
  "creates a typed regex dispatcher.  the type being
  '(:get :put :post :delete) and the regex being placed on the
  script-name of the call.  this is mostly similar to
  hunchentoot:create-regex-dispatcher."
  (let ((rd (hunchentoot:create-regex-dispatcher regex handler)))
    (lambda (request)
      (and (eq (hunchentoot:request-method*) method)
           (funcall rd request)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun components-to-regex* (components &optional (to-end "$"))
    "creates regex from the components of a url.
    to-end is the last portion of the url to be matched.  it defaults
    to \"$\", which means to the end of the line.  substituting it
    with \"\\\\?.*$\" will allow keys to be given near the end of the
    string."
    (format nil "^/*~{~a~^/+~}/*~A"
            (mapcar (lambda (x)
                      (if (stringp x) x "([^/]+)"))
                    components)
            to-end)))

(defun regex-match-list (regex string)
  "returns all submatches of <regex> when applied to <string> as
  a list."
  (multiple-value-bind (s e starts ends)
      (cl-ppcre:scan regex string)
    (declare (ignore s e))
    (loop for s across starts
       for e across ends
       collect (subseq string s e))))

(defun parse-script-name (components)
  "parses the contents of the current request as per
  <components>.  this entails searching for all matching content
  and placing it as variables in a hash.  all strings are
  considered fixpoints, all symbols are considered to be
  variables filled in by the current request."
  (let ((script-name (hunchentoot:script-name*))
        (symbols (remove-if-not #'symbolp components))
        (regex (components-to-regex* components))
        (variables-hash (make-hash-table :synchronized t)))
    (loop for symbol in symbols
       for value in (regex-match-list regex script-name)
       do
         (setf (gethash symbol variables-hash) value))
    variables-hash))

(defun parse-script-name-as-list (components)
  (multiple-value-bind (s e starts ends)
      (cl-ppcre:scan (components-to-regex components "\\\\?.*$")
                     (hunchentoot:script-name*))
    (declare (ignore s e))
    (loop for s across starts
       for e across ends
       collect (subseq (hunchentoot:script-name*) s e))))

(defun response-json-parser (response)
  "Parses the json for the response content by the matched
  method.
  If :no-content is returned, an empty response is returned
  otherwise the response is assumed to be jsown content."
  (if (eql response :no-content)
      ""
      (jsown:to-json response)))

(defun mk-response-method (components callback applied-parameters-p)
  "constructs a method which parses <components> and calls
  <callback> with a hash containing the variables of the call as
  per `parse-request-content'."
  (lambda ()
    (wait-for-page)
    (setf (hunchentoot:content-type*) "application/json")
    (if applied-parameters-p
        (response-json-parser
         (apply callback (parse-script-name-as-list components)))
        (funcall (alexandria:compose #'response-json-parser
                                     callback
                                     #'parse-script-name)
                 components))))

(defun specify-call (&key method components callback applied-parameters-p)
  "defines a json call functionally.

  - method :: contains a :get :put :post :delete symbol.
  - components :: contains a list of all components.  A string 
      indicates fixed content, a symbol indicates a variable.
  - callback :: function to be called to handle the request."
  (push (create-typed-regex-dispatcher method
                                       (components-to-regex* components)
                                       (mk-response-method components
                                                           callback
                                                           applied-parameters-p))
        hunchentoot:*dispatch-table*))

(defmacro defcall (method (&rest components) &body body)
  "defines a webpage, consisting of <components>"
  (let ((variables (remove-if #'keywordp components))
        (method-symbol (intern (symbol-name method) :keyword)))
    `(specify-call
       :method ,method-symbol
       :components ,`(quote ,components)
       :callback (lambda (,@variables) ,@body)
       :applied-parameters-p t)))

(defmacro with-parameters ((&rest parameters) &body body)
  `(let ,(loop for varname in parameters
            collect `(,varname (parameter ,(string-downcase (string varname)))))
     ,@body))

(defmacro defcatchall (&body body)
  "defines a catch-all page"
  `(alexandria:appendf hunchentoot:*dispatch-table*
                       (list (hunchentoot:create-regex-dispatcher
                              ".*" (lambda () ,@body)))))

;;;;;;;;;;;;;;;;;;;;
;;;; hosting folders
(defun mount (filesystem-folder server-url)
  "Mounts folder <filesystem-folder> on <server-url> as a mountpoint.
   The former is a local folder, the latter is a base url without / in
   the front and the back (this is added automagically).
   eg: (host-folder \"/tmp/downloaded-library/\" \"lib\")"
  (push (hunchentoot:create-folder-dispatcher-and-handler
         (format nil "/~a/" server-url) filesystem-folder)
        hunchentoot:*dispatch-table*))

;;;;;;;;;;;;;;;;;;;;;;;
;;; starting the server
(defparameter *v-acceptor* nil
  "hunchentoot-acceptor for this service")

(defun default-port ()
  "returns the default port for the current setup."
  (if (find :docker *features*) 80 8080))

(defun boot (&optional (port (default-port)))
  "hosts the service on localhost"
  (if *v-acceptor*
      (error "hunchentoot is already running")
      (progn
        (setf *v-acceptor* (make-instance 'hunchentoot:easy-acceptor
                                          :port port
                                          :access-log-destination nil))
        (hunchentoot:start *v-acceptor*))))

(defun toggle-logging ()
  "toggles the logging of the server on or off"
  (setf (hunchentoot:acceptor-access-log-destination *v-acceptor*)
        (if (hunchentoot:acceptor-access-log-destination *v-acceptor*)
            nil
            *error-output*)))

