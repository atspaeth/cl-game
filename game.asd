(defsystem "game"
  :description "game: my test Lisp game"
  :version "0.0.1"
  :author "Me!"
  :depends-on (:alexandria :sdl2 :sdl2-image :livesupport)
  :serial t
  :components ((:file "packages")
               (:file "game")))
