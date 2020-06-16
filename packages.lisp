
(defpackage :game/render
  (:use :cl :alexandria)
  (:export 
    :+screen-width+ :+screen-height+ 
    :load-texture-on :with-window-and-renderer
    :draw-atlas-frame :load-atlases
    :make-sprite :sprite-x :sprite-y :sprite-atlas-id
    :sprite-animation :sprite-frame-rate-ticks 
    :sprite-start-tick :sprite-flip? :sprite-draw))

(defpackage :game
  (:use :cl :alexandria :game/render)
  (:export :main)) 

