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
    (add-component :fish :bounding-box
                   :rect (sdl2:make-rect -30 25 60 30))
    (add-component :player :bounding-box
                   :rect (sdl2:make-rect -20 5 40 40))
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

; Define one player entity.
(add-component :player :sprite
               :atlas-id :player1
               :animation :stand
               :frame-rate-ticks 80)
(add-component :player :world-position
               :x (/ +screen-width+ 2.0)
               :y (/ +screen-height+ 2.0))


; Define one fly entity.
(add-component :fly :sprite
               :atlas-id :fly
               :animation :fly
               :frame-rate-ticks 80)
(add-component :fly :world-position
               :x -60.0 :y -70.0
               :parent (get-component :player :world-position))


(add-component :fish :sprite
               :atlas-id :fish
               :animation :swim
               :frame-rate-ticks 80)
(add-component :fish :world-position
               :x 456.0 :y 322.0)

(defcomponent bounding-box
  (rect (sdl2:make-rect 0 0 0 0)))

(defun bounding-box-draw (bbox renderer x y)
  (let ((rect (sdl2:copy-rect
                (bounding-box-rect bbox))))
    (incf (sdl2:rect-x rect) (round x))
    (incf (sdl2:rect-y rect) (round y))
    (sdl2:set-render-draw-color renderer #xCC #xCC #xCC #xCC)
    (sdl2:render-draw-rect renderer rect)))

(defparameter *debug-draw-bounding-boxen* t)

(defun renderables-sorted-by-y ()
  (sort
    (alist-of-entities-with '(:world-position :sprite))
    #'<
    :key (lambda (ec)
           (destructuring-bind (name pos sprite) ec
             (declare (ignore name sprite))
             (world-position-y pos)))))

(defun render-system (renderer)
  (loop for (id pos sprite) in (renderables-sorted-by-y) do
        (multiple-value-bind (x y) (resolve-world-position pos)
          (sprite-draw sprite renderer x y)))
  (when *debug-draw-bounding-boxen*
    (do-entities-with (id ((pos :world-position)
                           (bbox :bounding-box)))
      (multiple-value-bind (x y) (resolve-world-position pos)
        (bounding-box-draw bbox renderer x y)))))



(defparameter *player-speed* 0.2)

(defparameter *fish-speed* 0.1)

(defun get-world-rect (pos bbox)
  (lret ((rect (sdl2:copy-rect (bounding-box-rect bbox))))
    (multiple-value-bind (x y) (resolve-world-position pos)
      (incf (sdl2:rect-x rect) (round x))
      (incf (sdl2:rect-y rect) (round y)))))

(defun collision-vector (rect-a rect-b)
  "Return whether a collision occurred and the separation vector
as (values collision? dx dy)."
  (multiple-value-bind (collision? region)
      (sdl2:intersect-rect rect-a rect-b)
    (if (not collision?)
      (values nil 0 0)
      (let ((dx (sdl2:rect-width region))
            (dy (sdl2:rect-height region)))
        (if (< dx dy)
            (if (> (sdl2:rect-x rect-a)
                   (sdl2:rect-x rect-b))
                (values t dx 0)
                (values t (- dx) 0))
            (if (> (sdl2:rect-y rect-a)
                   (sdl2:rect-y rect-b))
                (values t 0 dy)
                (values t 0 (- dy))))))))


(defun resolve-collision (elist-a elist-b)
  "Push two entities apart if they're colliding."
  (nest
    (destructuring-bind (id-a pos-a bbox-a) elist-a)
    (destructuring-bind (id-b pos-b bbox-b) elist-b)
    (let ((rect-a (get-world-rect pos-a bbox-a))
          (rect-b (get-world-rect pos-b bbox-b))))
    (multiple-value-bind (collision? dx dy)
        (collision-vector rect-a rect-b))
    (when collision?
      (incf (world-position-x pos-a) (truncate dx 2))
      (decf (world-position-x pos-b) (truncate dx 2))
      (incf (world-position-y pos-a) (truncate dy 2))
      (decf (world-position-y pos-b) (truncate dy 2)))))



(defun resolve-collisions (entities)
  (loop for (entity . others) on entities do
        (loop for other in others do
              (resolve-collision entity other))))

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
                        '(:world-position :bounding-box))))

(defun draw-everything (renderer)
  "Render the world to the display."
  (sdl2:set-render-draw-color renderer #x33 #x33 #x33 #x33)
  (sdl2:render-clear renderer)
  (render-system renderer)
  (sdl2:render-present renderer))

