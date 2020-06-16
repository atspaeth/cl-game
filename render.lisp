(in-package :game/render)


;; SIMPLE SUPPORT BITS

(defconstant +screen-width+ 640
  "The width of the render target in pixels.")
(defconstant +screen-height+ 480
  "The height of the render target in pixels.")

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
                    (mod frame (length rects))
                    (min frame (length rects)))))
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

(defun load-atlas (renderer id filename)
  "Load a texture atlas from a file into the registry."
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


