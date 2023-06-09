#!/usr/bin/env sh

sbcl --load /opt/quicklisp/setup.lisp --script build-migrate.lisp --non-interactive
