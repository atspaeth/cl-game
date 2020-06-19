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


(defun renderables-sorted-by-y ()
  (sort
    (alist-of-entities-with '(:world-position :sprite))
    #'<
    :key (lambda (ec)
           (destructuring-bind (name pos sprite) ec
             (declare (ignore name sprite))
             (world-position-y pos)))))

(defparameter *debug-draw-bounding-boxen* t)

(defun render-system (renderer)
  (loop for (id pos sprite) in (renderables-sorted-by-y) do
        (sprite-draw sprite renderer
                     (world-position-x pos)
                     (world-position-y pos)))
  (when *debug-draw-bounding-boxen*
    (do-entities-with (id ((pos :world-position)
                           (bbox :box-collider)))
      (box-collider-draw bbox renderer
                     (world-position-x pos)
                     (world-position-y pos)))))



(defparameter *player-speed* 0.2)

(defparameter *fish-speed* 0.1)

(defun update-logic (dt-ms)
  "Step the behavior of the system."
  (with-sprite (get-component :fish :sprite)
    (with-slots (x y) (get-component :fish :world-position)
      (when (< x 64.0)
        (setf flip? t))
      (when (> x 564.0)
        (setf flip? nil))
      (incf x (* dt-ms *fish-speed* (if flip? 1 -1)))))
  (with-sprite (get-component :player :sprite)
    (multiple-value-bind (xaxis yaxis) (keyboard-arrow-position)
      (with-slots (x y) (get-component :player :world-position)
        (incf x (* xaxis dt-ms *player-speed*))
        ; Move slower in Y to give a sort of 2½-d effect.
        (incf y (* yaxis dt-ms *player-speed* 0.6)))
      (cond ((< xaxis 0) (setf flip? t))
            ((> xaxis 0) (setf flip? nil)))
      (if (not (= xaxis yaxis 0))
        (setf animation :walk)
        (setf animation :stand)))
    (setf (sprite-flip? (get-component :fly :sprite)) (not flip?)))
  (resolve-collisions (alist-of-entities-with
                        '(:world-position :box-collider))))

(defun draw-everything (renderer)
  "Render the world to the display."
  (sdl2:set-render-draw-color renderer #x33 #x33 #x33 #x33)
  (sdl2:render-clear renderer)
  (render-system renderer)
  (sdl2:render-present renderer))


; Define one player entity.
(add-component :player :sprite
               :atlas-id :player1
               :animation :stand
               :frame-rate-ticks 80)
(add-component :player :world-position
               :x (/ +screen-width+ 2.0)
               :y (/ +screen-height+ 2.0))
(add-component :player :box-collider
               :x 0.0 :y 25.0 :w 40.0 :h 40.0)

; Define one fly entity.
(add-component :fly :sprite
               :atlas-id :fly
               :animation :fly
               :frame-rate-ticks 80)
(add-component :fly :world-position
               :x 60.0 :y 70.0)


; Define one fish entity.
(add-component :fish :sprite
               :atlas-id :fish
               :animation :swim
               :frame-rate-ticks 80)
(add-component :fish :world-position
               :x 456.0 :y 322.0)
(add-component :fish :box-collider
               :x 0.0 :y 40.0 :w 60.0 :h 30.0 :mass 100.0)

