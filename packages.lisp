(defpackage #:game/util
  (:use :cl :alexandria)
  (:export :some-let*))

(defpackage :game/loader
  (:use :cl :alexandria :game/util)
  (:export :register-load-func :def-load-func :get-resource))

(defpackage :game
  (:use :cl :alexandria :game/loader :game/util)
  (:export :main)) 

