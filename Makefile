sources:=$(wildcard *.lisp)

game : $(sources)
	sbcl --eval "(ql:quickload '(:game :trivial-dump-core))" --eval "(trivial-dump-core:save-executable \"game\" (lambda () (sdl2:make-this-thread-main #'game:main)))"

swank :
	sbcl --load ~/.vim/pack/slimy-things/start/vlime/lisp/start-vlime.lisp --eval "(ql:quickload :game)" --eval "(sdl2:make-this-thread-main #'game:main)"
