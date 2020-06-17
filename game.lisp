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

(defun main ()
  (with-window-and-renderer (wnd renderer)
    (sdl2:set-render-draw-color renderer #x33 #x33 #x33 #x33)
    (load-atlases renderer
                  #P"Atlases/player1.atlas"
                  #P"Atlases/enemies.atlas")
    (with-dt-timer get-dt-ms
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
         (livesupport:continuable
           (livesupport:update-repl-link)
           (update-logic (get-dt-ms))
           (draw-everything renderer)))))))

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

(defvar *player* (make-sprite
                   :atlas-id :player1
                   :animation :stand
                   :frame-rate-ticks 80))
(defparameter *move-speed* 0.2)

(defvar *fly* (make-sprite
                :atlas-id :fly
                :animation :fly
                :frame-rate-ticks 80))

(defun update-logic (dt-ms)
  "Step the behavior of the system."
  (with-sprite *player*
    (multiple-value-bind (xaxis yaxis) (keyboard-arrow-position)
      (incf x (* xaxis dt-ms *move-speed*))
      ; Move slower in Y to give a sort of 2½-d effect.
      (incf y (* yaxis dt-ms *move-speed* 0.6))
      (cond ((< xaxis 0) (setf flip? t))
            ((> xaxis 0) (setf flip? nil)))
      (if (not (= xaxis yaxis 0))
        (setf animation :walk)
        (setf animation :stand)))
    (setf (sprite-x *fly*) (- x 30)
          (sprite-y *fly*) (- y 100))
    (setf (sprite-flip? *fly*) (not flip?))))

(defun draw-everything (renderer)
  "Render the world to the display."
  (sdl2:render-clear renderer)
  (sprite-draw *player* renderer)
  (sprite-draw *fly* renderer)
  (sdl2:render-present renderer))

