(in-package :game)

(defconstant +screen-width+ 640
  "The width of the render target in pixels.")
(defconstant +screen-height+ 480
  "The height of the render target in pixels.")

(defvar *test-atlas* nil
  "An example texture atlas.")

(defun load-texture-on (renderer filename)
  "Load a texture onto our render target using SDL_Image."
  (sdl2:create-texture-from-surface 
    renderer 
    (sdl2-image:load-image filename)))

(defmacro with-window-and-renderer ((window renderer) &body body)
  "Combine the SDL with-window and with-renderer macros for concision."
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                         :title "Maaaaagic"
                         :w +screen-width+ :h +screen-height+
                         :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 
                                      :flags '(:accelerated 
                                               :presentvsync))
         ,@body))))

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
    (setf *test-atlas* 
          (load-animation-atlas renderer "Atlases/p1_spritesheet.txt"))
    (with-dt-timer get-dt-ms
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
        (:idle ()
         (livesupport:continuable
           (livesupport:update-repl-link)
           (update-logic (get-dt-ms))
           (draw-everything renderer)))))))


(defun draw-texture-rect (renderer texture source-rect dest-x dest-y)
  "Blit a rectangle of a texture without stretching. Accepts float
coordinates, but rounds them to the nearest integer."
  (let ((dest-w (sdl2:rect-width source-rect))
        (dest-h (sdl2:rect-height source-rect)))
    (sdl2:render-copy renderer texture
                      :source-rect source-rect
                      :dest-rect (sdl2:make-rect 
                                   (round dest-x) (round dest-y) 
                                   dest-w dest-h))))

(defvar *frame-index* 0)
(defvar *pos-x* 0)
(defvar *pos-y* 0)

(defparameter *move-speed* 0.1)

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
    (incf *pos-y* (* yaxis dt-ms *move-speed*)))
  (incf *frame-index* 0.25)) 

(defun draw-everything (renderer)
  "Render the world to the display."
  (sdl2:render-clear renderer)
  (draw-animation-frame renderer *test-atlas* 
                        :walk (truncate *frame-index*)
                        *pos-x* *pos-y*)
  (sdl2:render-present renderer))


(defun read-one-from-file (filename)
  "Read the first lisp expression from a file, ignoring the rest."
  (with-open-file (file filename)
    (let ((*read-eval* nil))
      (read file))))

(defstruct animation-atlas
  "An atlas containing multiple different animations."
  texture
  frame-rects)

(defun animation-atlas-source-rect (atlas animation frame)
  "Return the source rect corresponding to a frame of an animation."
  (let ((rects (gethash animation 
                        (animation-atlas-frame-rects atlas))))
    (elt rects (mod frame (length rects)))))
        
(defun draw-animation-frame (renderer atlas animation frame dest-x dest-y)
  "Draw the specified frame of the specified animation from an atlas."
  (let ((rect (animation-atlas-source-rect atlas animation frame)))
    (draw-texture-rect renderer (animation-atlas-texture atlas)
                       rect dest-x dest-y)))

(defun load-animation-atlas (renderer filename)
  "Load a texture atlas from a file, returning an image name and 
the corresponding sequence of animation information."
  (let* ((spritesheet (read-one-from-file filename))
         (image-name (cadr spritesheet))
         (animations (cddr spritesheet))
         (frame-rects (make-hash-table)))
    (loop for anim in animations do
          (let ((name (cadr anim))
                (rects (caddr anim)))
            (setf (gethash name frame-rects)
                  (map 'vector 
                       (lambda (rect)
                         (apply #'sdl2:make-rect rect))
                       rects))))
    (make-animation-atlas :texture (load-texture-on renderer image-name) 
                          :frame-rects frame-rects)))

