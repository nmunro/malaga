(load "~/quicklisp/setup.lisp")
(ql:quickload :malaga)

(malaga/tools:migrate)
(malaga/tools:sync-scryfall-data)
