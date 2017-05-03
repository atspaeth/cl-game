(in-package :game)

(require :game/util "util.lisp")
(require :game/loader "resource-loader.lisp")

(eval-when (:compile-toplevel)
  (ql:quickload :sdl2)
  (ql:quickload :apply-argv))

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(register-load-func :bmp #'sdl2:load-bmp)
(get-resource :bg :bmp "hello_world.bmp")

(defun main (&key (delay 2000))
  (sdl2:with-init (:video)
    (sdl2:with-window (wnd :title "SDL2 Window"
			   :w *screen-width* :h *screen-height*)
      (let ((surf (sdl2:get-window-surface wnd)))
	(sdl2:blit-surface (get-resource :bg) nil surf nil)
	(sdl2:update-window wnd)
	(sdl2:delay delay)))))
