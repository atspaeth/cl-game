(in-package :game)

(defvar *entities* (make-hash-table)
  "A set of all the entities in the world. This is temporary.")
(defvar *component-registry* (make-hash-table)
  "Registry of component types, mapping each to its constructor.")

(defun register-component-type (name constructor)
  "Register a component type by adding its constructor to the registry."
  (setf (gethash name *component-registry*)
        (cons (make-hash-table) constructor)))

(defmacro defcomponent (name &body body)
  "Define a new component type. Just a decorated struct: register the
component type so new ones can be easily constructed."
  `(prog1
       (defstruct ,name ,@body)
     (register-component-type
       ,(make-keyword name)
       #',(symbolicate "MAKE-" name))))

(defun add-component (entity component-type
                             &rest component-args)
  "Add a component to a named entity. If the entity doesn't exist
already, silently create it. Also, the component may not be
preconstructed---since components are structs, the component
arguments should all be keywords. As a result, it's safe to assume
that if there is exactly one, it must be a preconstructed
component."
  ; Ensure the entity is included in the entities set.
  (setf (gethash entity *entities*) entity)
  ; Look up the component type and create a new component
  ; of that type inside the component table.
  (destructuring-bind (table . constructor)
      (gethash component-type *component-registry*)
    (setf (gethash entity table)
          (if (= (length component-args) 1)
            (car component-args)
            (apply constructor component-args)))))

(defun get-component (entity component-type)
  "Get the given type of component from the named entity, failing
silently if it's not available."
  (when-let (table (car (gethash component-type *component-registry*)))
    (gethash entity table)))

(define-do-macro do-entities-with
    ((id component-list &optional return) &body body)
  "Loop over all the entities with a given set of components, binding
each to a given name and executing the body."
  (with-gensyms (ignored)
    `(do-hash-table (,id ,ignored *entities*)
       (declare (ignore ,ignored))
       (when-let ,(loop for (name kind) in component-list collecting
                        (list name `(get-component ,id ,kind)))
         ,@body))))

(defun alist-of-entities-with (component-list)
  "Cons up an alist of all the entities with a given set of components.
A future refactor should make this disappear."
  (loop for entity being the hash-keys of *entities*
        when (loop for kind in component-list
                   if (get-component entity kind)
                   collect it into components else return nil
                   finally (return (cons entity components)))
        collecting it))



;; Object serialization!

(defun dump-object (entity)
  "Create an alist mapping the component types to structures for
this entity."
  (loop for component being the hash-keys of *component-registry*
        when (get-component entity component) collect it))

(defun dump-game ()
  "Produce an alist mapping entity IDs to dumped objects."
  (loop for entity being the hash-keys of *entities*
        collecting (cons entity (dump-object entity))))

(defun print-constructor-call (struct &optional (stream t))
  "Format a struct like a constructor call, by printing it and chopping
off the initial #S, but also make the structure type a keyword so it can
be looked up in the hashtable upon read."
  (format stream "(:~g" ;)
          (subseq
            (with-output-to-string (str)
              (prin1 struct str))
            3)))

(defun save-game (filename)
  "Save the objects produced by dump-game to a file."
  (with-output-to-file (*standard-output* filename
                                          :if-exists :supersede)
    (loop for (entity . components) in (dump-game) do
          (progn
            (format t "~%(~s" entity)
            (loop for component in components do
                  (format t "~%  ")
                  (print-constructor-call component))
            (format t ")")))
    (terpri)))

(defun undump-game (objects)
  "Empty the world of objects and install all the ones in this list."
  ; Get rid of all registered components.
  (loop for table-and-constructor being
        the hash-values of *component-registry* do
        (setf (car table-and-constructor)
              (make-hash-table)))
  ; Empty the set of entities.
  (setf *entities* (make-hash-table))
  ; Load each of the listed entities.
  (loop for (entity . components) in objects do
        (loop for (component . args) in components do
              (apply #'add-component entity component args))))

(defun load-objects (filename)
  "Load a list of objects from a file."
  (let ((*read-eval* nil))
    (with-input-from-file (in filename)
      (loop for object = (read in nil)
            while object collect object))))

(defun load-game (filename)
  "Load the full game state from a file."
  (undump-game (load-objects filename)))

;; The most fundamental component: a world transform.

(defcomponent world-position
  (x 0.0 :type float)
  (y 0.0 :type float))

