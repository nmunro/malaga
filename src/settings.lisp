(defpackage malaga/settings
  (:use :cl))

(in-package malaga/settings)

(setf (envy:config-env-var) "ENV")

(envy:defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :malaga))
    :authors ("NMunro <nmunro@duck.com>")
    :installed-apps ("barghest/auth" "barghest/admin" "trader")
    :package-delimiter "/" ; Here so that later devs don't have to use "/" as package separators
    :static-url "/static"
    :http-address ,(machine-instance)
    :http-port 5000))

(envy:defconfig |development|
  `(:database (:mysql
               :database-name ,(uiop:getenv "MALAGA_DB")
               :username ,(uiop:getenv "MALAGA_MYSQL_USERNAME")
               :password ,(uiop:getenv "MALAGA_MYSQL_PASSWORD")
               :port 3306)))
