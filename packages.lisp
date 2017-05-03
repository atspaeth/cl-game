(defpackage #:game/util
  (:use :common-lisp)
  (:export :let-some))

(defpackage :game/loader
  (:use :common-lisp)
  (:export :register-load-func :def-load-func :get-resource))

(defpackage :game
  (:use :common-lisp :game/loader :game/util)
  (:export :main :*resources*))
