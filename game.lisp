(in-package :game)

(defmacro with-dt-timer (name &body body)
  "Introduce a locally-scoped timer which records the time since
it was last queried."
  (with-gensyms (last-time this-time dt-ticks)
    `(let ((,last-time (sdl2:get-performance-counter)))
       (flet ((,name ()
                (let* ((,this-time (sdl2:get-performance-counter))
                       (,dt-ticks (- ,this-time ,last-time)))
                  (setf ,last-time ,this-time)
                  (/ (* 1000.0 ,dt-ticks)
                     (sdl2:get-performance-frequency)))))
         ,@body))))

(defvar *test-atlas* nil)

(defun main ()
  (with-window-and-renderer (wnd renderer)
    (sdl2:set-render-draw-color renderer #x33 #x33 #x33 #x33)
    (load-atlas renderer :p1 #P"Atlases/p1_spritesheet.txt")
    (with-dt-timer get-dt-ms
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
         (livesupport:continuable
           (livesupport:update-repl-link)
           (update-logic (get-dt-ms))
           (draw-everything renderer)))))))

(defvar *animation* :stand)
(defvar *frame-index* 0)
(defvar *pos-x* 0)
(defvar *pos-y* 0)
(defvar *flip-p* nil)
(defparameter *move-speed* 0.2)

(defun keyboard-arrow-position ()
  "Translate the keyboard cursor keys to an x,y pair."
  (flet ((key->int (scancode)
           (if (sdl2:keyboard-state-p scancode)
             1.0 0.0)))
    (let ((px (- (key->int :scancode-d) (key->int :scancode-a)))
          (py (- (key->int :scancode-s) (key->int :scancode-w))))
      (if (= (* px py) 0.0)
          (values px py)
          (values (/ px (sqrt 2))
                  (/ py (sqrt 2)))))))

(defun update-logic (dt-ms)
  "Step the behavior of the system."
  (multiple-value-bind (xaxis yaxis) (keyboard-arrow-position)
    (incf *pos-x* (* xaxis dt-ms *move-speed*))
    (incf *pos-y* (* yaxis dt-ms *move-speed*))
    ; Looks redundant but isn't - don't change facing without input.
    (when (< xaxis 0) (setf *flip-p* t))
    (when (> xaxis 0) (setf *flip-p* nil))
    (if (not (= xaxis yaxis 0))
      (setf *animation* :walk)
      (setf *animation* :stand)))
  (incf *frame-index* 0.25))

(defun draw-everything (renderer)
  "Render the world to the display."
  (sdl2:render-clear renderer)
  (draw-atlas-frame renderer :p1
                        *animation* (truncate *frame-index*)
                        *pos-x* *pos-y* :flip? *flip-p*)
  (sdl2:render-present renderer))

