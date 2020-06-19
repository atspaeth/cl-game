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

(define-do-macro do-entity-events ((event entity &optional return)
                                   &body body)
  "Perform some code for all the events in an entity's queue, then
empty the queue."
  `(when-let (queue (get-component ,entity :event-queue))
     (loop for ,event in (nreverse (event-queue-list queue))
           do ,@body
           finally (setf (event-queue-list queue) nil))))

