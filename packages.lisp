
(defpackage :game/render
  (:use :cl :alexandria)
  (:export 
    :+screen-width+ :+screen-height+ 
    :load-texture-on :with-window-and-renderer
    :draw-atlas-frame :load-atlas
    :draw-animation :make-animation))

(defpackage :game
  (:use :cl :alexandria :game/render)
  (:export :main)) 

