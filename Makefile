sources:=$(wildcard *.lisp)

game : $(sources)
	sbcl --eval "(ql:quickload '(:game :trivial-dump-core))" --eval "(trivial-dump-core:save-executable \"game\" (lambda () (sdl2:make-this-thread-main #'game:main)))"
