(in-package :game)

(defparameter +screen-width+ 640)
(defparameter +screen-height+ 480)

(defvar *test-image* nil)

(defun load-texture (renderer filename)
  (sdl2:create-texture-from-surface 
    renderer 
    (sdl2:load-bmp filename)))

(defmacro with-window-and-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:with-window (,window
                         :title "Maaaaagic"
                         :w +screen-width+ :h +screen-height+
                         :flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 
                                      :flags '(:accelerated))
         ,@body))))

(defun main ()
  (with-window-and-renderer (wnd renderer)
    (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
    (setf *test-image* 
          (load-texture renderer "test.bmp"))
    (sdl2:with-event-loop (:method :poll)
      (:quit () t)
      (:keydown (:keysym keysym)
       (case (sdl2:scancode keysym)
         (:scancode-escape
          (sdl2:push-event :quit))))
      (:idle ()
       (livesupport:continuable
         (livesupport:update-repl-link)
         (draw-everything renderer)
         (sdl2:delay 100))))))
                            
(defun draw-everything (renderer)
  (sdl2:render-clear renderer)
  (sdl2:render-copy renderer *test-image*)
  (sdl2:render-present renderer))
  

