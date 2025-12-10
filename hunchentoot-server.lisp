(in-package :webserver)

(defstruct (fake-connection (:constructor mk-fake-connection))
  (return-code 200)
  (headers-in nil)
  (headers-out nil)
  (get-parameters nil)
  (request-method :get)
  (script-name ""))

(defun make-fake-connection-from-current ()
  (mk-fake-connection
   :return-code (hunchentoot:return-code*)
   :headers-in (hunchentoot:headers-in*)
   :headers-out (hunchentoot:headers-out*)
   :get-parameters (hunchentoot:get-parameters*)
   :request-method (request-method*)
   :script-name (script-name*)))

(defparameter *fake-connection* nil
  "When non-nil, get headers etc from this object instead.

This object allows to fake the connection in situations where the hunchentoot
connection is already closed.")

(defmacro with-fake-connection (fake-connection &body body)
  "Executes BODY with the context of a fake connection."
  `(let ((*fake-connection* ,fake-connection))
     ,@body))

(defun (setf return-code*) (code)
  "Sets the return code."
  (if *fake-connection*
      (setf (fake-connection-return-code *fake-connection*) code)
      (setf (hunchentoot:return-code*) code)))

(defun header-in* (header)
  "Retrieves the input header with HEADER being a keyword."
  (if *fake-connection*
      (cdr (assoc header (fake-connection-headers-in *fake-connection*)))
      (hunchentoot:header-in* header)))

(defun header-out (header)
  "Gets the header for the response with HEADER being a keyword."
  (if *fake-connection*
      (cdr (assoc header (fake-connection-headers-out *fake-connection*)))
      (hunchentoot:header-out header)))

(defun (setf header-out) (value header)
  "Sets one of the headers for the response."
  (if *fake-connection*
      (let ((entry (assoc header (fake-connection-headers-out *fake-connection*))))
        ;; adapted standard pattern from hunchentoot
        (if entry
            (setf (cdr entry) header)
            (setf (fake-connection-headers-out *fake-connection*)
                  (acons header value (fake-connection-headers-out *fake-connection*)))))
      (setf (hunchentoot:header-out header) value))
  value)

(defun get-parameter (parameter)
  "Retrieves the supplied get-parameter."
  (if *fake-connection*
      (cdr (assoc parameter (fake-connection-get-parameters *fake-connection*) :test #'string=))
      (hunchentoot:get-parameter parameter)))

(defun get-parameters* ()
  "Retrieves all get parameters"
  (hunchentoot:get-parameters*))

(defun script-name* ()
  "Retrieves the URI without the request parameters"
  (hunchentoot:script-name*))

(defun post-body ()
  "Retrieves the current post-body and parses it as text."
  (hunchentoot:raw-post-data :external-format :utf8 :force-text t))

(defun request-method* ()
  "Retrieves the request method of the current request."
  (hunchentoot:request-method*))

(defconstant +http-no-content+ hunchentoot:+http-no-content+)
(defconstant +http-not-found+ hunchentoot:+http-not-found+)
(defconstant +http-not-acceptable+ hunchentoot:+http-not-acceptable+)
(defconstant +http-forbidden+ hunchentoot:+http-forbidden+)
(defconstant +http-conflict+ hunchentoot:+http-conflict+)
(defconstant +http-created+ hunchentoot:+http-created+)
