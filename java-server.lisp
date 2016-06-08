(in-package :webserver)

;;;;;;;;;;;;;;;;
;;;; Java wiring
(defparameter *request* nil
  "Bound to the current request when a request is made.")
(defparameter *response* nil
  "Bound to the current response when a request is made.")

(defclass request (ah:java-class)
  ((url :accessor url
        :java-field "url")
   (query-parameters :accessor query-parameters
                     :java-field "queryParameters")
   (headers :accessor headers
            :java-field "headers")
   (method :accessor method
           :java-field "method")
   (body :accessor body
         :java-field "body"))
  (:metaclass ah:java-metaclass)
  (:java-class "com.tenforce.esco.communication.Request"))

(defclass response (ah:java-class)
  ((url :accessor url
        :java-field "url")
   (headers :accessor headers
            :java-field "headers")
   (method :accessor method
           :java-field "method")
   (return-code :accessor return-code
                :java-field "returnCode")
   (body :accessor body
         :java-field "body"))
  (:metaclass ah:java-metaclass)
  (:java-class "com.tenforce.esco.communication.Response"))


;;;;;;;;;;;;;;;;;;;
;;;; Nano framework
(defclass page-matcher ()
  ((regex :accessor regex
          :initarg :regex)
   (method :accessor method
           :initarg :method)
   (handler :accessor handler
            :initarg :handler))
  (:documentation "Matches a page and can hanlde it"))

(defparameter *page-handlers* nil)

(defun add-page-matcher (method regex handler)
  "Adds a new page matcher"
  (push (make-instance 'page-matcher :method method :handler handler :regex regex)
        *page-handlers*))

(defun page-matcher-regex-and-vars (specification)
  (values (format nil "^/~{~:[(~A)~;~A~]~^/~}/?$"
                  (loop for word in specification
                     if (keywordp word)
                     append `(t ,(string-downcase (symbol-name word)))
                     else
                     append `(nil ".*")))
          (loop for word in specification
             unless (keywordp word)
             collect word)))

(defmacro defcall (method (&rest specification) &body body)
  "Defines a page in our rediculous framework"
  (multiple-value-bind (regex vars)
      (page-matcher-regex-and-vars specification)
    `(add-page-matcher ,method
                       ,regex
                       (lambda (url)
                         (apply (lambda (,@vars) ,@body)
                                (map 'list #'identity
                                     (second (multiple-value-list
                                              (cl-ppcre:scan-to-strings ,regex url)))))))))

;; ;; TODO: deprecated
;; (defmacro defpage ((&rest specification) &body body)
;;   "Defines a page in our rediculous framework"
;;   (multiple-value-bind (regex vars)
;;       (page-matcher-regex-and-vars specification)
;;     `(add-page-matcher :get
;;                        ,regex
;;                        (lambda (url)
;;                          (apply (lambda (,@vars) ,@body)
;;                                 (map 'list #'identity
;;                                      (second (multiple-value-list
;;                                               (cl-ppcre:scan-to-strings ,regex url)))))))))

(defun find-page (method url)
  "searches for the right page in the set of urls"
  (loop for page-matcher in *page-handlers*
     when
       (and (string= (string method) (method page-matcher))
            (cl-ppcre:scan (regex page-matcher) url))
     return page-matcher))

(defun handle-request (method url)
  "ensures the right handler handled the request for URL"
  (handler-case
      (let ((page-matcher (find-page method url)))
        (if page-matcher
            (funcall (handler page-matcher) url)
            "Page not found"))
    (error () "An error occurred :(")))

(defun perform-request (request)
  "Executes a request from the Java world."
  (let ((*request* (make-instance 'request
                                  :java-instance request))
        (*response* (make-instance 'response)))
    (let ((response (jsown:to-json (handle-request (method *request*) (url *request*)))))
      (setf (body *response*) response))
    (ah:java-instance *response*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Page helper implementation
(defun (setf return-code*) (code)
  "Sets the return code of the response."
  (setf (return-code *response*) code))

(defun header-in* (header)
  "Retrieves the input header from the request."
  (java-get (headers *request*) (string-downcase (string header))))

(defun (setf header-out) (value header)
  "Sets one of the headers for the response."
  (java-put (headers *response*) (string-downcase (string header)) value))

(defun get-parameter (parameter)
  "Retrieves the supplied get-parameter."
  (let ((parameters (coerce (java-get (query-parameters *request*) parameter) 'list)))
    (values (first parameters)
            parameters)))

(defun get-parameters* ()
  "Retrieves a plist of all query parameters."
  (loop for key in (java-hash-keys (query-parameters *request*))
     append
       (loop for value in (second (multiple-value-list (get-parameter key)))
          collect `(,key . ,value))))

(defun script-name* ()
  "Retrieves the script name, this is the url without the hostname and query parameters."
  (url *request*))

(defun post-body ()
  "Retrieves the body which was posted by the user."
  (body *request*))

(defun request-method* ()
  "Retrieves the request method of the current request as a symbol."
  (intern (method *request*) (find-package :keyword)))

(defun (setf content-type*) (content-type)
  (set-content-type *response* content-type))

(ah:import-java-method java-get (key)  ; used on maps
                       :java-method "get")
(ah:import-java-method java-put (key value)  ; used on maps
                       :java-method "put")
(ah:import-java-method set-content-type (content-type))

(defun java-hash-keys (hash-map-instance)
  (coerce (java:jcall "toArray" (java:jcall "keySet" hash-map-instance)) 'list))


(defconstant +http-no-content+ 204)
(defconstant +http-not-found+ 404)
(defconstant +http-not-acceptable+ 406)
(defconstant +http-forbidden+ 403)
(defconstant +http-conflict+ 409)
(defconstant +http-created+ 201)
