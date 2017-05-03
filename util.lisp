(in-package :game/util)


;; A cross-breed of let* and cond:
;;  like let*, but returns nil as soon as a variable is bound to nil.
;; Alternately, like as-> but gives a separate name to each intermediate.
(defmacro let-some (let-exprs &body body)
  (loop for (name val) in (reverse let-exprs)
	with expr = body do
	  (setf expr
		`((let ((,name ,val))
		    (when ,name
		      ,@expr))))
	  finally (return (car expr))))

