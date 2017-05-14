(in-package :game/loader)

;; The hashtable of all the resources that have been loaded.
(defparameter *resources* (make-hash-table))

;; The resource-loading functions, which take a path and bonus args
;;  and return a pointer to the resource that has been loaded.
(defparameter *type-load-funcs* ())
(defun register-load-func (type func)
  (push (cons type func) *type-load-funcs*))
(defmacro def-load-func (type arglist &body body)
  `(register-load-func ,type (lambda ,arglist ,@body)))
(defun unregister-load-func (type)
  (some-> type
	  (assoc *type-load-funcs*)
	  cdr
	  (setf nil)))

;; Load a resource from a file and store it in the resource table.
(defun load-from-file (name type path &rest args)
  (some-let* ((load-func (cdr (assoc type *type-load-funcs*)))
	 (resource (apply load-func path args)))
    (setf (gethash name *resources*) resource)))

;; Retrieve a resource by name. 
;; If it hasn't been loaded yet and a type and path are provided,
;;  load it from disk.
(defun get-resource (name type &optional path &rest args)
  (or
   (gethash name *resources*)
   (and path (apply #'load-from-file name type path args))))


