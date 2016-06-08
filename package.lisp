#-java-backend
(defpackage :webserver
  (:use :cl)
  (:import-from :hunchentoot :*request*)
  (:export :return-code*
           :header-in*
           :get-parameter
           :get-parameters*
           :script-name*
           :post-body
           :request-method*
           :content-type*
           :header-out
           :defcall
           :+http-no-content+
           :+http-not-found+
           :+http-not-acceptable+
           :+http-forbidden+
           :+http-conflict+
           :+http-created+))

#+java-backend
(defpackage :webserver
  (:use :cl)
  (:export :perform-request
           :return-code*
           :header-in*
           :get-parameter
           :get-parameters*
           :script-name*
           :post-body
           :request-method*
           :content-type*
           :header-out
           :defcall
           :+http-no-content+
           :+http-not-found+
           :+http-not-acceptable+
           :+http-forbidden+
           :+http-conflict+
           :+http-created+))

(defpackage :mu-support
  (:use :cl)
  (:import-from :fuseki
                :fuseki-server :fuseki-repository
                :long-query :query :insert
                :add-prefix)
  (:import-from :webserver
                :defcall
                :post-body)
  (:export :s+ :clean-url :clean-string :make-uuid
           :post-body :defcall :specify-call :boot
           :*repository*
           :fuseki-server :fuseki-repository
           :long-query :query :insert
           :add-prefix
           :s-url :s-var :s-from-json :s-str :s-prefix :s-escaped :s-typed :sparql-escape :raw-content
           :s{} :s-graph :s-select :s-delete :s-insert :s-inv :s-inv-p
           :s-genvar :s-values))
