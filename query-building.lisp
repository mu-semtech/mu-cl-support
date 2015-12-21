(in-package :mu-support)

;;;;;;;;;;;;;;;;;;;
;;;; query building
;;
;; Provides support for the construction of queries in a format
;; which allows us to correctly escape content and which should
;; not be too obtrusive to the end-user.

(defclass sparql-content ()
  ((content :accessor raw-content :initarg :content))
  (:documentation "Primitive on top of which content which may be
   rendered are constructed."))

(defclass sparql-escaped-content (sparql-content)
  ()
  (:documentation "Represents content which is already escaped"))

(defclass sparql-url (sparql-content)
  ()
  (:documentation "Represents a longer formatted url"))

(defclass sparql-variable (sparql-content)
  ()
  (:documentation "Represents a variable in a SPARQL query"))

(defclass sparql-string (sparql-content)
  ()
  (:documentation "Represents a string in a SPARQL query"))

(defclass sparql-prefixed (sparql-content)
  ()
  (:documentation "Represents prefixed sparql content"))

(defclass sparql-typed-literal (sparql-content)
  ((literal-type :accessor literal-type :initarg :literal-type))
  (:documentation "Represents a typed literal."))

(defclass sparql-inverse (sparql-content)
  ()
  (:documentation "Represents the inverse relationship of its content."))

(defgeneric sparql-escape (content)
  (:documentation "Formats <content> so it can be rendered inside
   a query.  This handles all necessary escaping.")
  (:method ((escaped sparql-escaped-content))
    (raw-content escaped))
  (:method ((url sparql-url))
    (s+ "<" (clean-url (raw-content url)) ">"))
  (:method ((var sparql-variable))
    ;; I think variables have more constraints than urls (spacing)
    (s+ "?" (clean-url (raw-content var))))
  (:method ((string sparql-string))
    (s+ "\"" (clean-string (raw-content string)) "\""))
  (:method ((resource sparql-prefixed))
    ;; I think prefixes should have similar constraints as variables, no?
    (clean-url (raw-content resource)))
  (:method ((resource sparql-typed-literal))
    (s+ (sparql-escape (s-str (raw-content resource))) "^^" (sparql-escape (literal-type resource))))
  (:method ((resource sparql-inverse))
    (let ((content (raw-content resource)))
      (if (typep content (find-class 'sparql-inverse))
          (sparql-escape (raw-content content))
          (s+ "^" (sparql-escape content))))))

(defmethod print-object ((content sparql-content) stream)
  (format stream "~A" (sparql-escape content)))

(defun s-url (string)
  (make-instance 'sparql-url :content string))

(defun s-var (string)
  (make-instance 'sparql-variable :content string))

(defun s-str (string)
  (let ((escaped (cl-ppcre:regex-replace-all
                  (string #\Newline) string "\\n")))
   (make-instance 'sparql-string :content escaped)))

(defun s-typed (string type)
  (make-instance 'sparql-typed-literal
                 :content string
                 :literal-type type))

(defun s-bool (string)
  (s-typed string (s-prefix "typedLiterals:boolean")))

(defun s-inv (content)
  (make-instance 'sparql-inverse :content content))

(defun s-inv-p (content)
  "returns non-nil iff content is an inverse predicate."
  (typep content (find-class 'sparql-inverse)))

(defun s-from-json (content)
  (cond ((rationalp content)
         (coerce content 'float))
        ((stringp content)
         (s-str content))
        ((numberp content)
         content)
        (t (s-bool content))))

(defun s-prefix (string &optional content)
  (make-instance 'sparql-prefixed
                 :content (if content
                              (s+ string ":" content)
                              string)))

(defun s-escaped (string)
  (make-instance 'sparql-escaped-content :content string))

(defun s{} (&rest body)
  "Constructs a graph pattern between {} for all content in body."
  (format nil "{~{~&~4t~A~}~&}" body))

(defun s-select (parameters options &rest body)
  "Constructs a SELECT statement.
Options is a list which may contain the key :group-by for grouping
by a specific property."
  (destructuring-bind (&key order-by) options
    (s+ "SELECT " parameters
        " WHERE " (apply #'s{} body)
        (if order-by (format nil " ORDER BY ~A" order-by) ""))))

(defun s-delete (clauses &optional where)
  "Constructs a DELETE statement."
  (if where
      (format nil "DELETE ~A WHERE ~A"
              (s{} clauses)
              (s{} where))
      (format nil "DELETE WHERE ~&~A"
              (s{} clauses))))

(defun s-insert (&rest body)
  "Constructs an INSERT DATA statement."
  (format nil "INSERT DATA ~&~A~&"
          (apply #'s{} body)))

(defun s-graph (graph &rest body)
  "Constructs a GRAPH statement."
  (format nil "GRAPH ~A ~A"
          graph (apply #'s{} body)))
