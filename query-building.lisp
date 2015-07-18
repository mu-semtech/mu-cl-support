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
  (:documentation "represents prefixed sparql content"))

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
    (clean-url (raw-content resource))))

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

(defun s-from-json (content)
  (if (numberp content)
      (if (rationalp content)
          (coerce content 'float)
          content)
      (s-str content)))

(defun s-prefix (string &optional content)
  (make-instance 'sparql-prefixed
                 :content (if content
                              (s+ string ":" content)
                              string)))

(defun s-escaped (string)
  (make-instance 'spralq-escaped-content :content string))
