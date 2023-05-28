(load "~/quicklisp/setup.lisp")
(ql:quickload :malaga)

(malaga/tools:migrate)
(malaga/tools:sync-player-data)
