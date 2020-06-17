(in-package :game)

;; SIMPLE SUPPORT BITS

(defconstant +screen-width+ 640
  "The width of the render target in pixels.")
(defconstant +screen-height+ 480
  "The height of the render target in pixels.")

(let (last-renderer texture-table)
  (defun load-texture-on (renderer filename)
    "Load a texture onto our render target."
    ; If the render target has changed, all textures are invalidated.
    (unless (eq renderer last-renderer)
      (setf texture-table (make-hash-table)))
    (or
      (gethash filename texture-table)
      (setf (gethash filename texture-table)
        (sdl2:create-texture-from-surface
          renderer
          (sdl2-image:load-image filename))))))

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




;; LOCAL CODE FOR TEXTURE ATLASES


(defstruct atlas
  "An atlas containing multiple different animations."
  texture
  frame-rects)

(defvar *atlas-registry* (make-hash-table))

(defun atlas-source-rect (atlas animation frame &key (cycle? t))
  "Return the source rect corresponding to a frame of an animation."
  (let* ((rects (gethash animation (atlas-frame-rects atlas)))
         (frame (if cycle?
                    (mod (truncate frame) (length rects))
                    (min (truncate frame) (length rects)))))
    (elt rects frame)))

(defun draw-texture-rect (renderer texture source-rect
                                   dest-x dest-y
                                   &key flip?)
  "Blit a rectangle of a texture without stretching. Accepts float
coordinates, but rounds them to the nearest integer. The position
is interpreted as the position of the center."
  (let* ((dest-w (sdl2:rect-width source-rect))
         (dest-h (sdl2:rect-height source-rect))
         (dest-x (round (- dest-x (/ dest-w 2))))
         (dest-y (round (- dest-y (/ dest-h 2)))))
    (sdl2:render-copy-ex renderer texture
                         :source-rect source-rect
                         :dest-rect (sdl2:make-rect
                                     dest-x dest-y
                                     dest-w dest-h)
                         :flip (if flip? '(:horizontal)))))


;; EXPORTED API FOR TEXTURE ATLASES

(defun draw-atlas-frame (renderer atlas-id animation frame dest-x dest-y
                                  &key (flip? nil) (cycle? t))
  "Draw the specified frame of the specified animation from an atlas."
  (let* ((atlas (gethash atlas-id *atlas-registry*))
         (rect (atlas-source-rect atlas animation frame :cycle? cycle?)))
    (draw-texture-rect renderer (atlas-texture atlas)
                       rect dest-x dest-y
                       :flip? flip?)))

(defun load-atlas (renderer filename)
  "Load the first texture atlas from a file into the registry."
  (let* ((spritesheet (with-open-file (file filename)
                        (let ((*read-eval* nil))
                          (read file))))
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
    (setf
      (gethash id *atlas-registry*)
      (make-atlas :texture (load-texture-on renderer image-name)
                  :frame-rects frame-rects))))

(defun import-atlas (renderer spritesheet)
  "Given a single atlas expression, import it to the registry."
  (let* ((id (car spritesheet))
         (texture-path (cadr spritesheet))
         (animations (cddr spritesheet))
         (rect-table (make-hash-table)))
    (loop for anim in animations do
          (let ((name (car anim))
                (rect-list (cadr anim)))
            (setf (gethash name rect-table)
                  (map 'vector
                       (lambda (rect)
                         (apply #'sdl2:make-rect rect))
                       rect-list))))
    (setf (gethash id *atlas-registry*)
          (make-atlas :texture (load-texture-on
                                 renderer texture-path)
                      :frame-rects rect-table))))

(defun load-atlases (renderer &rest filenames)
  "Load texture atlases from a file into the registry."
  (loop for filename in filenames do
        (with-open-file (file filename)
          (loop for spritesheet =
                (let ((*read-eval* nil)
                      (*package* (find-package 'keyword)))
                  (read file nil))
                while spritesheet do
                (import-atlas renderer spritesheet)))))




;; EXPORTED API FOR ANIMATED SPRITES

(defstruct sprite
  "An animated sprite."
  (atlas-id nil :type symbol)
  (animation nil :type symbol)
  (frame-rate-ticks 100 :type fixnum)
  (start-tick (sdl2:get-ticks) :type fixnum)
  (flip? nil :type boolean))

(defmacro with-sprite (sprite &body body)
  "Avoid with-slots for this largish struct."
  `(with-slots (atlas-id animation
                frame-rate-ticks start-tick flip?)
       ,sprite
     ,@body))

(defun sprite-draw (sprite renderer x y)
  "Draw the current animation frame of a sprite to the renderer."
  (with-sprite sprite
    (draw-atlas-frame renderer atlas-id animation
                      (/ (- (sdl2:get-ticks) start-tick)
                         frame-rate-ticks)
                      x y :flip? flip?)))

