(in-package :game/util)

(defmacro some-let* (let-exprs &body body)
  "A cross-breed of let* and cond: like let*, but returns nil as 
soon as a variable is bound to nil."
  (loop for (name val) in (reverse let-exprs)
        with expr = body do
        (setf expr
              `((let ((,name ,val))
                  (when ,name
                    ,@expr))))
    finally (return (car expr))))

