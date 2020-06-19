(in-package :game)

(defcomponent bounding-box
  "A bounding box in coordinates relative to the object's transform.
The x and y coordinates actually refer to the *center* of the box."
  (x 0.0 :type float)
  (y 0.0 :type float)
  (w 0.0 :type float)
  (h 0.0 :type float))

(defun bounding-box-left (bbox)
  (with-slots (x w) bbox
    (- x (/ w 2))))

(defun bounding-box-right (bbox)
  (with-slots (x w) bbox
    (+ x (/ w 2))))

(defun bounding-box-top (bbox)
  (with-slots (y h) bbox
    (- y (/ h 2))))

(defun bounding-box-bottom (bbox)
  (with-slots (y h) bbox
    (+ y (/ h 2))))

(defun bounding-box-draw (bbox renderer screen-x screen-y)
  (with-slots (w h) bbox
    (sdl2:with-rects ((rect (round (+ screen-x (bounding-box-left bbox)))
                            (round (+ screen-y (bounding-box-top bbox)))
                            (round w)
                            (round h)))
      (sdl2:set-render-draw-color renderer #xCC #xCC #xCC #xCC)
      (sdl2:render-draw-rect renderer rect))))

(defun get-world-rect (pos bbox)
  "Translate a bounding box to world coordinates."
  (with-slots (x y w h) bbox
    (make-bounding-box
      :x (+ x (world-position-x pos))
      :y (+ y (world-position-y pos))
      :w w :h h)))

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
              (bounding-box-left rect-a)
              (bounding-box-right rect-a)
              (bounding-box-left rect-b)
              (bounding-box-right rect-b)))
        (dy (intervals-overlap
              (bounding-box-top rect-a)
              (bounding-box-bottom rect-a)
              (bounding-box-top rect-b)
              (bounding-box-bottom rect-b))))
    (if (< (abs dx) (abs dy))
        (values (not (zerop dx)) dx 0.0)
        (values (not (zerop dy)) 0.0 dy))))

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
      (incf (world-position-x pos-a) (/ dx 2))
      (decf (world-position-x pos-b) (/ dx 2))
      (incf (world-position-y pos-a) (/ dy 2))
      (decf (world-position-y pos-b) (/ dy 2)))))

(defun resolve-collisions (entities)
  (loop for (entity . others) on entities do
        (loop for other in others do
              (resolve-collision entity other))))

