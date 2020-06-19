(defsystem "game"
  :description "game: my test Lisp game"
  :version "0.0.1"
  :author "Me!"
  :depends-on (:alexandria :sdl2 :sdl2-image
               :livesupport :serapeum)
  :serial t
  :components ((:file "package")
               (:file "ecs")
               (:file "render")
               (:file "collision")
               (:file "game")))
