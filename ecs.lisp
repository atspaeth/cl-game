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
  (gethash entity
    (car (gethash component-type
                  *component-registry*))))

(define-do-macro do-entities-with
    ((id component-list &optional return) &body body)
  (with-gensyms (ignored)
    `(do-hash-table (,id ,ignored *entities*)
       (declare (ignore ,ignored))
       (when-let ,(loop for (name kind) in component-list collecting
                        (list name `(get-component ,id ,kind)))
         ,@body))))

;; The most fundamental component: a world transform.

(defcomponent world-position
  (x 0.0 :type float)
  (y 0.0 :type float)
  (parent nil :type symbol))

(defun get-world-position (entity)
  (when-let (pos (get-component entity :world-position))
    (with-slots (x y parent) pos
      (if (no parent) (values x y)
          (multiple-value-bind (px py)
              (get-world-position parent)
            (values (+ x px) (+ y py)))))))

