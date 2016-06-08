(in-package :abcl-helpers)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun string-capitalize* (string)
    "Turns the first character of the string into a capital letter"
    (when (> (length string) 0)
      (concatenate 'string
                   (string-upcase (subseq string 0 1))
                   (string-downcase (subseq string 1))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun string-camelcase (string &optional uppercase-first-character)
    "Turns a string from snailcase into camelcase"
    (let ((sequences
           (mapcar #'string-capitalize*
                   (split-sequence:split-sequence #\- string))))
      (unless uppercase-first-character
        (setf (first sequences)
              (string-downcase (first sequences))))
      (apply #'concatenate 'string sequences)))

  (defun package-camelcase (string)
    "Turns the package string into a package string which Java would understand"
    (apply #'concatenate 'string
           (mapcar #'string-camelcase
                   (split-sequence:split-sequence #\. string)))))

;; Note: this should really use the MOP
(defclass java-metaclass (c2mop:standard-class)
  ((java-class :accessor java-class))
  (:documentation "metaclass for java classes"))

(defmethod c2mop:validate-superclass ((a java-metaclass) (b c2mop:standard-class))
  t)

(defclass java-class ()
  ((java-instance :reader java-instance
                  :initarg :java-instance))
  (:documentation "superclass for java classes"))

(defclass java-direct-slot (c2mop:standard-direct-slot-definition)
  ((java-property-type :accessor java-property-type
                       :initarg :java-type)
   (java-property-field :accessor java-property-field
                        :initarg :java-field)
   (java-property-reader :accessor java-property-reader
                         :initarg :java-reader
                         :initform nil)
   (java-property-writer :accessor java-property-writer
                         :initarg :java-writer
                         :initform nil))
  (:documentation "Contains the meta-info for storing and retrieving the slot from Java"))

(defclass java-effective-slot (c2mop:standard-effective-slot-definition)
  ((direct-slot :initarg :direct-slot
                :accessor direct-slot))
  (:documentation "An effective slot that contains its linked direct slot."))

(defmethod initialize-instance :around ((instance java-class) &key &allow-other-keys)
  (unless (and (slot-boundp instance 'java-instance)
               (slot-value instance 'java-instance))
    (setf (slot-value instance 'java-instance)
          (java:jnew (java-class (class-of instance)))))
  (call-next-method))

(defmethod initialize-instance :around ((slot java-direct-slot) &rest args &key name java-accessor java-reader java-writer java-field readers writers)
  (unless java-field
    (setf java-field (string-camelcase name))
    (alexandria:appendf args `(:java-field ,java-field)))
  (when (and readers (not java-reader))
    (setf java-reader (format nil "get~A" (string-camelcase (symbol-name (or java-accessor name)) t)))
    (alexandria:appendf args `(:java-reader ,java-reader)))
  (when (and writers (not java-writer))
    (setf java-writer (format nil "set~A" (string-camelcase (symbol-name (or java-accessor name)) t)))
    (alexandria:appendf args `(:java-writer ,java-writer)))
  (apply #'call-next-method slot args))

;; slot magic
(defparameter *direct-slots* nil "Special variable to send the direct slots to the effective-slot-definition-class method")
(defgeneric initialize-extra-info (slot args)
  (:documentation "Initializes the extra information that an effective slot could get from the direct slots"))
(defmethod initialize-extra-info ((slot java-effective-slot) args)
  (setf (direct-slot slot) (first args))
  slot)
(defmethod initialize-extra-info (a b)
  (declare (ignore b))
  a)
(defmethod c2mop:compute-effective-slot-definition :around ((class java-metaclass) name args)
  (let ((*direct-slots* args))
    (initialize-extra-info (call-next-method) args)))
(defmethod c2mop:effective-slot-definition-class :around ((class java-metaclass) &rest args)
  (declare (ignore args))
  (cond ((find (find-class 'java-direct-slot) *direct-slots* :key #'class-of)
         (find-class 'direct-effective-slot))
        (t
         (call-next-method))))
;; slot magic

(defparameter *slots* nil)

(defgeneric initialize-slot-methods (class slot)
  (:documentation "Allows the slot to initialize its generic functions and methods")
  (:method (class slot)
    t)
  (:method ((class java-metaclass) (slot java-effective-slot))
    (let* ((direct-slot (direct-slot slot))
           (reader (first (c2mop:slot-definition-readers direct-slot)))
           (writer (first (c2mop:slot-definition-writers direct-slot)))
           (java-reader (java-property-reader direct-slot))
           (java-writer (java-property-writer direct-slot)))
      (when java-reader
        (let ((gf (ensure-generic-function reader
                                           :lambda-list '(instance))))
          (add-method gf
                      (make-instance (c2mop:generic-function-method-class gf)
                                     :specializers `(,class)
                                     :lambda-list '(instance)
                                     :function (lambda (instance)
                                                 (java:jcall java-reader (java-instance instance)))))))
      (when java-writer
        (let ((gf (ensure-generic-function writer
                                           :lambda-list '(value instance))))
          (add-method gf
                      (make-instance (c2mop:generic-function-method-class gf)
                                     :specializers `(,(find-class t) ,class)
                                     :lambda-list '(value instance)
                                     :function (lambda (value instance)
                                                 (java:jcall java-writer
                                                             (java-instance instance)
                                                             value)))))))))

(defmethod shared-initialize :around ((jmc java-metaclass) slot-names &rest args &key java-class &allow-other-keys)
  (declare (ignore args))
  (setf (java-class jmc) (first java-class))
  (let ((result (call-next-method)))
    (setf *slots* (c2mop:class-slots jmc))
    (mapcar (alexandria:curry #'initialize-slot-methods jmc) (c2mop:class-slots jmc))
    result))

(defmethod c2mop:direct-slot-definition-class :around ((jmc java-metaclass) &rest args &key (java-type nil java-type-p) (java-field nil java-field-p) &allow-other-keys)
  (declare (ignore args java-type java-field))
  (if (or java-type-p java-field-p)
      (find-class 'java-direct-slot)
      (call-next-method)))

(defmethod c2mop:effective-slot-definition-class :around ((jmc java-metaclass) &rest args)
  (declare (ignore args))
  (if (find (find-class 'java-direct-slot) *direct-slots* :key #'class-of)
      (find-class 'java-effective-slot)
      (call-next-method)))

(defmethod c2mop:slot-value-using-class :around ((jmc java-metaclass) (instance java-class) (slot java-effective-slot))
  (java:jfield (java-class jmc) (java-property-field (direct-slot slot)) (java-instance instance)))

(defmethod (setf c2mop:slot-value-using-class) :around (value (jmc java-metaclass) (instance java-class) (slot java-effective-slot))
  (setf (java:jfield (java-class jmc) (java-property-field (direct-slot slot)) (java-instance instance))
        value))

(defgeneric unwrap-lisp-object (instance)
  (:documentation "Unwraps a lisp object so it can be read in Java.")
  (:method (instance)
    instance)
  (:method ((instance java-class))
    (java-instance instance)))

(defmacro import-java-function (java-class functor)
  "Imports a function from Java into ABCL"
  (let ((function-string (string-camelcase (symbol-name functor))))
    `(defmethod ,functor (&rest args)
       (apply #'java:jstatic ,function-string ,java-class
              (mapcar #'unwrap-lisp-object args)))))

(defmacro import-java-method (method-name (&rest args) &key java-method)
  "Imports the specified java method"
  (let ((java-method-name (or java-method (string-camelcase (symbol-name method-name))))
        (instance-sym (gensym "instance")))
    `(defun ,method-name (,instance-sym ,@args)
       (apply #'java:jcall
              ,java-method-name
              (unwrap-lisp-object ,instance-sym)
              (mapcar #'unwrap-lisp-object (list ,@args))))))

;;;;;;;;;;;;;;;;
;; example usage

;; (defclass request (java-class)
;;   ((url :initarg :url
;;         :accessor url
;;         :java-field "url"
;;         :java-reader url
;;         :java-type "java.lang.String")
;;    (method :initarg :method
;;            :accessor method
;;            :java-reader method
;;            :java-field "method"
;;            :java-type "java.lang.String"))
;;   (:metaclass java-metaclass)
;;   (:documentation "Represents a Java request")
;;   (:java-class "com.tenforce.test.HttpRequest"))

;; (import-java-function "com.tenforce.test.Main" print-http-request)
;; (import-java-method print ())
;; (import-java-method send-to (user))

;; (defclass communication (java-class)
;;   ()
;;   (:metaclass java-metaclass)
;;   (:documentation "Make a test call")
;;   (:java-class "ch.semte.mu.communication.Communication"))

;; (import-java-method test-call ())
