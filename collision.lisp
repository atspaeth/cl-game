(in-package :game)

(defcomponent box-collider
  "A bounding box in coordinates relative to the object's transform.
The x and y coordinates actually refer to the *center* of the box."
  (x 0.0 :type float)
  (y 0.0 :type float)
  (w 0.0 :type float)
  (h 0.0 :type float)
  (mass 1.0 :type float))

(defun box-collider-left (bbox)
  (with-slots (x w) bbox
    (- x (/ w 2))))

(defun box-collider-right (bbox)
  (with-slots (x w) bbox
    (+ x (/ w 2))))

(defun box-collider-top (bbox)
  (with-slots (y h) bbox
    (- y (/ h 2))))

(defun box-collider-bottom (bbox)
  (with-slots (y h) bbox
    (+ y (/ h 2))))

(defun box-collider-draw (bbox renderer screen-x screen-y)
  (with-slots (w h) bbox
    (sdl2:with-rects ((rect (round (+ screen-x (box-collider-left bbox)))
                            (round (+ screen-y (box-collider-top bbox)))
                            (round w)
                            (round h)))
      (sdl2:set-render-draw-color renderer #xCC #xCC #xCC #xCC)
      (sdl2:render-draw-rect renderer rect))))

(defun get-world-rect (pos bbox)
  "Translate a bounding box to world coordinates."
  (with-slots (x y w h mass) bbox
    (make-box-collider
      :x (+ x (world-position-x pos))
      :y (+ y (world-position-y pos))
      :w w :h h :mass mass)))

(defun intervals-overlap (a b c d)
  "The signed overlap between the intervals (a b) and (c d), i.e. the
smallest amount by which you could translate the first interval to end
the overlap."
  (if (or (<= d a) (>= c b)) 0.0
    ; Whichever of these two differences is absolutely smallest
    ; is the direction in which it's easier to separate the two.
    ; These signs are already known because of the previous line.
    (if (< (- d a) (- b c))
        (- d a)
        (- c b))))

(defun collision-vector (rect-a rect-b)
  "Given two rects in world coordinates, return whether they
intersect, and the separation vector as (values collision? dx dy)."
  (let ((dx (intervals-overlap
              (box-collider-left rect-a)
              (box-collider-right rect-a)
              (box-collider-left rect-b)
              (box-collider-right rect-b)))
        (dy (intervals-overlap
              (box-collider-top rect-a)
              (box-collider-bottom rect-a)
              (box-collider-top rect-b)
              (box-collider-bottom rect-b))))
    (if (< (abs dx) (abs dy))
        (values (not (zerop dx)) dx 0.0)
        (values (not (zerop dy)) 0.0 dy))))

(defun resolve-collision (elist-a elist-b)
  "Push two entities apart if they're colliding."
  (nest
    (destructuring-bind (id-a pos-a bbox-a) elist-a
      (declare (ignore id-a)))
    (destructuring-bind (id-b pos-b bbox-b) elist-b
      (declare (ignore id-b)))
    (let ((rect-a (get-world-rect pos-a bbox-a))
          (rect-b (get-world-rect pos-b bbox-b))))
    (multiple-value-bind (collision? dx dy)
        (collision-vector rect-a rect-b))
    (when collision?
      (let* ((ma (box-collider-mass bbox-a))
             (mb (box-collider-mass bbox-b))
             (a (/ mb (+ ma mb)))
             (b (/ ma (+ ma mb))))
        (incf (world-position-x pos-a) (* dx a))
        (decf (world-position-x pos-b) (* dx b))
        (incf (world-position-y pos-a) (* dy a))
        (decf (world-position-y pos-b) (* dy b))))))


(defun resolve-collisions (entities)
  (loop for (entity . others) on entities do
        (loop for other in others do
              (resolve-collision entity other))))

