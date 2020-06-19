(in-package :game)

(register-component-type :event-queue #'list)

(defcomponent event-queue
  "Just a decorated list of events, treated as a FIFO queue."
  list)

(defun event-push (target event-name &rest parameters)
  "If the named entity has an event queue, push an event to it."
  (when-let (queue (get-component target :event-queue))
    (push (cons event-name parameters)
          (event-queue-list queue))))

(defmacro entity-events-case (entity &body clauses)
  "Pop all events from the named entity's queue in FIFO order, case
match each one by type, and execute the body with bindings."
  (with-gensyms (queue event)
    `(when-let (,queue (get-component ,entity :event-queue))
       (loop for ,event in (nreverse (event-queue-list ,queue))
             do (single-event-case
                  ,event
                  ,@clauses)
             finally (setf (event-queue-list ,queue) nil)))))

(defmacro single-event-case (event &body clauses)
  "Pattern match on a single event."
  `(case (car ,event)
     ,@(loop for (type bindings . body) in clauses collecting
             `(,type
                (destructuring-bind ,bindings (cdr ,event)
                  ,@body)))))

