(load "/opt/quicklisp/setup.lisp")
(ql:quickload :malaga)

(sb-ext:save-lisp-and-die "sync-scryfall-data" :executable t :toplevel 'malaga/tools:sync-scryfall-data)
