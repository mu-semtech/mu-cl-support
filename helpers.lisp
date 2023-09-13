(in-package :mu-support)

;;; helpers
(defun s+ (&rest strings)
  "Concatenates a set of strings"
  (apply #'concatenate 'string "" strings))

(defun string-replace (str from to)
  "replaces <from> with <to> in <str> for every regex occurence
  of <from>."
  (cl-ppcre:regex-replace-all from str to))
(define-compiler-macro string-replace (&whole whole str from to &environment env)
  (if (constantp from env)
      `(cl-ppcre:regex-replace-all ,from ,str ,to)
      whole))

(defun clean-url (url)
  "Cleans the supplied URL."
  (string-replace (string-replace url "<" "&lt;")
                  ">" "&gt;"))

(defun clean-string (string)
  "Cleans the supplied string"
  (string-replace
   (string-replace
    (string-replace string "\\" "\\\\\\\\")
    "\"" "\\\"")
   #.(string #\Newline) "\\n"))

(defun make-uuid ()
  "Creates a new UUID"
  (mongoid:oid-str (mongoid:oid)))
