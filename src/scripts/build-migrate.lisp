(load "~/quicklisp/setup.lisp")
(ql:quickload :malaga)

(sb-ext:save-lisp-and-die "migrate" :executable t :toplevel 'malaga/tools:migrate)
