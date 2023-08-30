(defpackage malaga/trader/urls
  (:use :cl)
  (:export #:patterns))

(in-package malaga/trader/urls)

(defparameter patterns (list
    (barghest/routes:path "" #'malaga/trader/views::index :method :GET :name :index)
    (barghest/routes:path "register" #'malaga/trader/views::register :method :GET :name :register)
    (barghest/routes:path "register" #'malaga/trader/views::register-user :method :POST :name :register-user)
    (barghest/routes:path "cards" #'malaga/trader/views::cards :method :GET :name :cards)
    (barghest/routes:path "cards/:card" #'malaga/trader/views::card :method :GET :name :card)
    (barghest/routes:path "players" #'malaga/trader/views::players :method :GET :name :players)
    (barghest/routes:path "players/:player/profile" #'malaga/trader/views::player :method :GET :name :profile)
    (barghest/routes:path "players/:player/cards" #'malaga/trader/views::player-cards :method :GET :name :player-cards)))
