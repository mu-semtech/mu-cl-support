(defpackage :mu-support
  (:use :cl)
  (:import-from :fuseki
                :fuseki-server :fuseki-repository
                :long-query :query :insert
                :add-prefix)
  (:export :s+ :clean-url :clean-string
           :post-body :defcall :specify-call :boot
           :*repository*
           :fuseki-server :fuseki-repository
           :long-query :query :insert
           :add-prefix
           :s-url :s-var :s-from-json :s-str :s-prefix :s-escaped :sparql-escape :raw-content))
