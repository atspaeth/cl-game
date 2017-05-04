(in-package :game)

(require :game/util "util.lisp")
(require :game/loader "resource-loader.lisp")

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)
(defparameter *screen-pixel-format* 0)

(def-load-func :bmp (path &optional (pixel-format *screen-pixel-format*))
  (-> path
      sdl2:load-bmp
      (sdl2:convert-surface-format pixel-format)))

(defun main ()
  (sdl2:with-init (:video)
    (sdl2:with-window (wnd :title "SDL2 Window"
			   :w *screen-width* :h *screen-height*
			   :flags '(:shown))
      (let* ((screen (sdl2:get-window-surface wnd))
	     (*screen-pixel-format* (sdl2:surface-format-format screen)))
	(get-resource :bg :bmp "hello_world.bmp")
	(sdl2:with-event-loop (:method :poll)
	  (:quit () t)
	  (:keydown (:keysym keysym)
		    (case (sdl2:scancode keysym)
		      (:scancode-escape
		       (sdl2:push-event :quit))))
	  (:idle ()
		 (sdl2:blit-surface (get-resource :bg :bmp) nil screen nil)
		 (sdl2:update-window wnd)
		 (sdl2:delay 100)))))))
