#!/usr/bin/env sh

sbcl --load /opt/quicklisp/setup.lisp --script build-sync-scryfall-data.lisp --non-interactive
