(in-package :webserver)

(defun (setf return-code*) (code)
  "Sets the return code."
  (setf (hunchentoot:return-code*) code))

(defun header-in* (header)
  "Retrieves the input header."
  (hunchentoot:header-in* header))

(defun (setf header-out) (value header)
  "Sets one of the headers for the response."
  (setf (hunchentoot:header-out header) value))

(defun get-parameter (parameter)
  "Retrieves the supplied get-parameter."
  (hunchentoot:get-parameter parameter))

(defun get-parameters ())

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

(defun (setf content-type*) (content-type)
  "Sets the content-type of the response."
  (setf (hunchentoot:content-type*) content-type))

(defconstant +http-no-content+ hunchentoot:+http-no-content+)
(defconstant +http-not-found+ hunchentoot:+http-not-found+)
(defconstant +http-not-acceptable+ hunchentoot:+http-not-acceptable+)
(defconstant +http-forbidden+ hunchentoot:+http-forbidden+)
(defconstant +http-conflict+ hunchentoot:+http-conflict+)
(defconstant +http-created+ hunchentoot:+http-created+)
