#!/bin/sh

source ../../.env
sbcl --script sync-player-data.lisp --non-interactive
