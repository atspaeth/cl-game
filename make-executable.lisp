(ql:quickload '(:game :trivial-dump-core))
(trivial-dump-core:save-executable 
  "game"
  (lambda ()
    (sdl2:make-this-thread-main #'game:main)))
                                   
