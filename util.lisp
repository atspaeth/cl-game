(in-package :game/util)



;; Pythonesque range: returns integers from a to b inclusive, or
;;  if b is not provided, a integers starting at 0 (from 0 to a-1).
(defun range (a &optional b)
  (if b
    (loop for i from a to b collecting i)
    (loop for i from 0 to (- a 1) collecting i)))

;; Clojure threading operators.
;; These take a list of expressions and compose them in a way useful for
;;  writing functional-style programs without so much indentation.
;; ->   thread-first  each expr is the first argument of the next
;; ->>  thread-last   each expr is the last argument of the next
;; as-> thread-as     provide a name where the argument will be threaded
;; In all three, a symbol not in a list will be assumed to be a function
;;  and called on the previous result.
(defmacro -> (&body body)
  (loop for expr in body
     for clause = expr then
       (if (listp expr)
	   `(,(car expr) ,clause ,@(cdr expr))
	   `(,expr ,clause))
       finally (return clause)))
(defmacro ->> (&body body)
  (loop for expr in body
     for clause = expr then
       (if (listp expr)
	   `(,@expr ,clause)
	   `(,expr ,clause))
       finally (return clause)))
(defmacro as-> (name &body body)
  (loop for expr in body
     for clause = expr then
       (if (listp expr)
	   `(let ((,name ,clause))
	      ,expr)
	   `(,expr ,clause))
       finally (return clause)))
(defmacro some-as-> (name &body body)
  (loop for expr in body
     for clause = expr then
       (if (listp expr)
	   `(let ((,name ,clause))
	      (when ,name ,expr))
	   `(let ((,name ,clause))
	      (when ,name (,expr ,name))))
       finally (return clause)))



;; A cross-breed of let* and cond:
;;  like let*, but returns nil as soon as a variable is bound to nil.
;; Alternately, like as-> but gives a separate name to each intermediate.
(defmacro some-let* (let-exprs &body body)
  (loop for (name val) in (reverse let-exprs)
	with expr = body do
	  (setf expr
		`((let ((,name ,val))
		    (when ,name
		      ,@expr))))
	  finally (return (car expr))))

