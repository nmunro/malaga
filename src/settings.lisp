(defparameter authors '("NMunro <nmunro@duck.com>"))

(defparameter installed-apps '("trader"))

(defparameter static-url "/static")

(defparameter databases `(
    :mysql
    :database-name ,(uiop:getenv "MALAGA_DB")
    :username ,(uiop:getenv "MALAGA_MYSQL_USERNAME")
    :password ,(uiop:getenv "MALAGA_MYSQL_PASSWORD")
    :port 3306))

(defparameter http-address (machine-instance))
(defparameter http-port 5000)
