(in-package :game)

(defvar *entities* (make-hash-table))
(defvar *component-registry* (make-hash-table))

(defun register-component-type (name constructor)
  (setf (gethash name *component-registry*)
        (cons (make-hash-table) constructor)))

(defmacro defcomponent (name &body body)
  `(prog1
       (defstruct ,name ,@body)
     (register-component-type
       ,(make-keyword name)
       #',(symbolicate "MAKE-" name))))

(defun add-component (entity component-type
                             &rest component-args)
  ; Ensure the entity is included in the entities set.
  (setf (gethash entity *entities*) entity)
  ; Look up the component type and create a new component
  ; of that type inside the component table.
  (destructuring-bind (table . constructor)
      (gethash component-type *component-registry*)
    (setf (gethash entity table)
          (apply constructor component-args))))

(defun get-component (entity component-type)
  (when-let (table (car (gethash component-type *component-registry*)))
    (gethash entity table)))

(define-do-macro do-entities-with
    ((id component-list &optional return) &body body)
  (with-gensyms (ignored)
    `(do-hash-table (,id ,ignored *entities*)
       (declare (ignore ,ignored))
       (when-let ,(loop for (name kind) in component-list collecting
                        (list name `(get-component ,id ,kind)))
         ,@body))))

(defun alist-of-entities-with (component-list)
  "Cons up an alist of all the entities with a given set of components."
  (loop for entity being the hash-keys of *entities*
        when (loop for kind in component-list
                   if (get-component entity kind)
                   collect it into components else return nil
                   finally (return (cons entity components)))
        collecting it))



;; The most fundamental component: a world transform.

(defcomponent world-position
  (x 0.0 :type float)
  (y 0.0 :type float))

