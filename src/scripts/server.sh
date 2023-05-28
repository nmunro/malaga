#!/bin/sh

source ../../.env
sbcl --script server.lisp --non-interactive
