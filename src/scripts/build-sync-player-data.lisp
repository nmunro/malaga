(load "~/quicklisp/setup.lisp")
(ql:quickload :malaga)

(sb-ext:save-lisp-and-die "sync-player-data" :executable t :toplevel 'malaga/tools:sync-player-data)
