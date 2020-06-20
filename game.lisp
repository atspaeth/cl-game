(in-package :game)

(defmacro with-dt-timer (name &body body)
  "Introduce a locally-scoped timer which records the time in
seconds since it was last queried."
  (with-gensyms (last-time this-time dt-ticks)
    `(let ((,last-time (sdl2:get-performance-counter)))
       (flet ((,name ()
                (let* ((,this-time (sdl2:get-performance-counter))
                       (,dt-ticks (- ,this-time ,last-time)))
                  (setf ,last-time ,this-time)
                  (/ (float ,dt-ticks)
                     (sdl2:get-performance-frequency)))))
         ,@body))))

(defun main ()
  (with-window-and-renderer (wnd renderer)
    (load-atlases renderer
                  #P"Atlases/player1.atlas"
                  #P"Atlases/enemies.atlas")
    (with-dt-timer get-dt
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
         (livesupport:continuable
           (livesupport:update-repl-link)
           (update-logic (get-dt))
           (draw-everything renderer)))))))

(defun keyboard-arrow-position ()
  "Translate the keyboard cursor keys to an x,y pair."
  (flet ((key->int (scancode)
           (if (sdl2:keyboard-state-p scancode)
             1.0 0.0)))
    (let ((px (- (key->int :scancode-d) (key->int :scancode-a)))
          (py (- (key->int :scancode-s) (key->int :scancode-w))))
      (values px py))))
      ; (if (= (* px py) 0.0)
      ;     (values px py)
      ;     (values (/ px (sqrt 2))
      ;             (/ py (sqrt 2)))))))

(defun keyboard-is-jumping ()
  "Check if the jump key is down."
  (sdl2:keyboard-state-p :scancode-space))


(defparameter *player-speed* 200.0)

(defparameter *player-acceleration* 800.0)

(defparameter *fish-speed* 100.0)

(defparameter *jump-base-speed* 500.0)

(defparameter *gravity* 1000.0)

(defmacro towardsf (place target amount)
  "For a setfable place which currently contains a number, move that
value towards some target by at most some amount."
  (with-gensyms (diff)
    `(let ((,diff (- ,target ,place)))
       (if (< (abs ,diff) ,amount)
         (setf ,place ,target)
         (incf ,place (* ,amount (signum ,diff)))))))

(defun update-logic (dt)
  "Step the behavior of the system."
  ; Hack for livecoding: if a frame takes more than a second, we
  ; probably did something weird and shouldn't actually be updating.
  (when (> dt 1.0)
    (return-from update-logic))
  (with-sprite (get-component :fish :sprite)
    (with-slots (x y) (get-component :fish :world-position)
      (when (< x 64.0)
        (setf flip? t))
      (when (> x 564.0)
        (setf flip? nil))
      (incf x (* dt *fish-speed* (if flip? 1 -1)))))
  ; General player updates.
  (nest
    (with-sprite (get-component :player :sprite))
    (with-slots (dxdt dydt) (get-component :player :box-collider))
    (let ((on-ground nil))
      (entity-events-case :player
        (:collision (other dx dy)
         (when (< dy 0) (setf on-ground t))
         (when (eq other :fish)
           (incf dxdt (* 100.0 dx))
           (incf dydt (* 100.0 dy)))))
      (towardsf dxdt (* *player-speed* (keyboard-arrow-position))
                (* dt *player-acceleration*))
      (if (and on-ground (keyboard-is-jumping))
        (setf dydt (- *jump-base-speed*))
        (incf dydt (* *gravity* dt)))
      ; Flip the sprite when going left.
      (cond ((< dxdt 0) (setf flip? t))
            ((> dxdt 0) (setf flip? nil)))
      ; Animation is walking unless we're standing or jumping.
      (setf animation (cond ((not on-ground) :jump)
                            ((not (zerop dxdt)) :walk)
                            ((sdl2:keyboard-state-p :scancode-s) :duck)
                            (t :stand)))))
  ; Update all velocities.
  (do-entities-with (entity ((bbox :box-collider)
                             (pos :world-position)))
    (with-slots (x y) pos
      (with-slots (dxdt dydt) bbox
        (incf x (* dxdt dt))
        (incf y (* dydt dt)))))
  (resolve-collisions (alist-of-entities-with
                        '(:world-position :box-collider))))


(defun renderables-sorted-by-y ()
  (sort
    (alist-of-entities-with '(:world-position :sprite))
    #'<
    :key (lambda (ec)
           (let ((pos (second ec)))
             (world-position-y pos)))))

(defparameter *debug-draw-bounding-boxen* t)

(defun draw-everything (renderer)
  "Render the world to the display."
  (sdl2:set-render-draw-color renderer #x33 #x33 #x33 #x33)
  (sdl2:render-clear renderer)
  (loop for (id pos sprite) in (renderables-sorted-by-y) do
        (sprite-draw sprite renderer
                     (world-position-x pos)
                     (world-position-y pos)))
  (when *debug-draw-bounding-boxen*
    (do-entities-with (id ((pos :world-position)
                           (bbox :box-collider)))
      (box-collider-draw bbox renderer
                     (world-position-x pos)
                     (world-position-y pos))))
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
               :x 0.0 :y 5.0 :w 40.0 :h 80.0)
(add-component :player :event-queue)

; Define one fly entity.
(add-component :fly :sprite
               :atlas-id :fly
               :animation :fly
               :frame-rate-ticks 80)
(add-component :fly :world-position
               :x 60.0 :y 70.0)

; Define a floor.
(add-component :floor :world-position
               :x (/ +screen-width+ 2.0)
               :y (float +screen-height+))
(add-component :floor :box-collider
               :w (float +screen-width+)
               :h 20.0 :mass -1.0)


; Define one fish entity.
(add-component :fish :sprite
               :atlas-id :fish
               :animation :swim
               :frame-rate-ticks 80)
(add-component :fish :world-position
               :x 456.0 :y 322.0)
(add-component :fish :box-collider
               :x 0.0 :y 2.5 :w 60.0 :h 30.0 :mass 10.0)

