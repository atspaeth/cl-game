(defpackage #:game/util
  (:use :common-lisp)
  (:export :some-let* :with-gensyms
	   :-> :->> :as-> :some-as-> :some-> :some->>
	   :range))

(defpackage :game/loader
  (:use :common-lisp :game/util)
  (:export :register-load-func :def-load-func :get-resource))

(defpackage :game
  (:use :common-lisp :game/loader :game/util)
  (:export :main :*resources*))
