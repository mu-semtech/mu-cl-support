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

(defclass sparql-multiple-values (sparql-content)
  ()
  (:documentation "Represents content which consists of multiple values"))

(defclass sparql-escaped-content (sparql-content)
  ()
  (:documentation "Represents content which is already escaped"))

(defclass sparql-url (sparql-content)
  ()
  (:documentation "Represents a longer formatted url"))

(defclass sparql-variable (sparql-content)
  ()
  (:documentation "Represents a variable in a SPARQL query"))

(defclass sparql-distinct (sparql-content)
  ()
  (:documentation "Represents a distinct in a SPARQL query"))

(defclass sparql-string (sparql-content)
  ()
  (:documentation "Represents a string in a SPARQL query"))

(defclass sparql-lang-string (sparql-content)
  ((language :accessor language :initarg :language))
  (:documentation "Represents a language-typed string in a SPARQL query"))

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
  (:method ((values sparql-multiple-values))
    (format nil "~{~A~^, ~}"
            (mapcar #'sparql-escape (raw-content values))))
  (:method ((url sparql-url))
    (s+ "<" (clean-url (raw-content url)) ">"))
  (:method ((var sparql-variable))
    ;; I think variables have more constraints than urls (spacing)
    (s+ "?" (clean-url (raw-content var))))
  (:method ((distinct sparql-distinct))
    (s+ "DISTINCT " (sparql-escape (raw-content distinct))))
  (:method ((string sparql-string))
    (s+ "\"\"\"" (clean-string (raw-content string)) "\"\"\""))
  (:method ((string sparql-lang-string))
    (s+ (sparql-escape (make-instance 'sparql-string :content (raw-content string)))
        "@" (language string)))
  (:method ((resource sparql-prefixed))
    ;; I think prefixes should have similar constraints as variables, no?
    (clean-url (raw-content resource)))
  (:method ((resource sparql-typed-literal))
    (s+ (sparql-escape (s-str (raw-content resource))) "^^" (sparql-escape (literal-type resource))))
  (:method ((resource sparql-inverse))
    (let ((content (raw-content resource)))
      (cond ((typep content (find-class 'sparql-inverse))
             (sparql-escape (raw-content content)))
            ((and (listp content) (> (length content) 1))
             (format nil "^(~{~A~^/~})" content))
            ((listp content)
             (s+ "^" (sparql-escape (first content))))
            (t (s+ "^" (sparql-escape content)))))))

(defmethod print-object ((content sparql-content) stream)
  (format stream "~A" (sparql-escape content)))

(defun s-url (string)
  (make-instance 'sparql-url :content string))

(defun s-var (string)
  (make-instance 'sparql-variable :content string))

(defun s-distinct (var)
  "DISTINCT wrapper for a sparql variable."
  (make-instance 'sparql-distinct :content var))

(defun s-str (string &optional language)
  (if language
      (make-instance 'sparql-lang-string
                     :content string
                     :language (escape-language language))
      (make-instance 'sparql-string :content string)))

(defun s-values (&rest objects)
  (make-instance 'sparql-multiple-values :content objects))

(defun escape-language (language)
  "Escapes the language string <language>."
  (cl-ppcre:scan-to-strings "\\w+" language))

(defun s-typed (string type)
  (make-instance 'sparql-typed-literal
                 :content string
                 :literal-type type))

(defun s-bool (content)
  (let ((string-representation
         (if (or (not content)
                (eq content :false))
             "false" "true")))
    (s-typed string-representation
             (s-prefix "typedLiterals:boolean"))))

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
  (destructuring-bind (&key order-by limit offset group-by) options
    (s+ "SELECT " (format nil "~A" parameters)
        " WHERE " (apply #'s{} body)
        (if group-by (format nil " GROUP BY ~A" group-by) "")
        (if order-by (format nil " ORDER BY ~A" order-by) "")
        (if offset (format nil " OFFSET ~A" offset) "")
        (if limit (format nil " LIMIT ~A" limit) ""))))

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

(let ((gennr 0))
  (defun s-genvar (&optional (name "gensym"))
    "Constructs an unused variable for a graph within the current thread."
    (let ((clean-name (cl-ppcre:regex-replace-all "[\\.\\-]" name "_")))
      (s-var (format nil "__~A~A" clean-name (incf gennr))))))

(defgeneric full-uri (item)
  (:documentation "Returns a full URL of a resource constructed as
    a URL, or by using a prefix.  Also understands lists and other sparql
    constructs.")
  (:method ((url sparql-url))
    (raw-content url))
  (:method ((prefixed sparql-prefixed))
    (destructuring-bind (prefix content)
        (split-sequence:split-sequence #\: (raw-content prefixed))
      (let ((base-uri (cl-fuseki:get-prefix prefix)))
        (unless base-uri
          (error 'simple-error :format-control "Prefix ~A could not be found." :format-arguments (list prefix)))
        (format nil "~A~A" base-uri content))))
  (:method ((items list))
    (mapcar #'full-uri items))
  (:method ((sparql-content sparql-content))
    (full-uri (raw-content sparql-content)))
  (:method ((anything t))
    anything))

(defgeneric same-uri (item-a item-b)
  (:documentation "returns t if both URIs are the same, because they
    represent the same full URI if both would be represented as a URI.")
  (:method ((pref-a sparql-prefixed) (pref-b sparql-prefixed))
    (string= (raw-content pref-a)
             (raw-content pref-b)))
  (:method ((uri-a sparql-url) (uri-b sparql-url))
    (string= (raw-content uri-a)
             (raw-content uri-b)))
  (:method ((pref-a sparql-prefixed) (uri-b sparql-url))
    (same-uri (full-uri pref-a) uri-b))
  (:method ((uri-a sparql-url) (pref-b sparql-prefixed))
    (same-uri uri-a (full-uri pref-b))))
