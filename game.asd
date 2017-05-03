(defsystem "game"
  :description "game: my test Lisp game"
  :version "0.0.1"
  :author "Me!"
  :depends-on ("sdl2" "apply-argv")
  :components ((:file "packages")
	       (:file "util" :depends-on ("packages"))
	       (:file "resource-loader" :depends-on ("util"))
	       (:file "game" :depends-on ("resource-loader" "util"))))
