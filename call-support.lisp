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

(defmacro defcall (method (&rest components) &body body)
  "defines a webpage, consisting of <components>"
  (let ((variables (remove-if #'keywordp components))
        (method-symbol (intern (symbol-name method) :keyword)))
    `(push (create-typed-regex-dispatcher
            ,method-symbol
	    ,(components-to-regex components)
	    (lambda ()
          (wait-for-page)
	      (multiple-value-bind (s e starts ends)
		  (cl-ppcre:scan ,(components-to-regex components "\\\\?.*$")
				 (hunchentoot:request-uri*))
		(declare (ignore s e))
                (setf (hunchentoot:content-type*) "application/json")
		(jsown:to-json
                 (apply (lambda (,@variables)
			 ,@body)
		       (loop for s across starts
			  for e across ends
			  collect (subseq (hunchentoot:request-uri*) s e)))))))
	   hunchentoot:*dispatch-table*)))

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
