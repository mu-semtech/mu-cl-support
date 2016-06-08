(in-package :mu-support)

;;; helpers
(defun s+ (&rest strings)
  "Concatenates a set of strings"
  (apply #'concatenate 'string "" strings))

(defun string-replace (str from to)
  "replaces <from> with <to> in <str> for every regex occurence
  of <from>."
  (cl-ppcre:regex-replace-all from str to))

(defun clean-url (url)
  "Cleans the supplied URL."
  (string-replace (string-replace url "<" "&lt;")
                  ">" "&gt;"))

(defun clean-string (string)
  "Cleans the supplied string"
  (string-replace
   (string-replace string "\\" "\\\\\\\\")
   "\"" "\\\""))

(defun make-uuid ()
  "Creates a new UUID"
  #+java-backend
  (format nil "~A" (uuid:make-v4-uuid))
  #-java-backend
  (mongoid:oid-str (mongoid:oid)))
