#!/bin/sh

source ../../.env
sbcl --script sync-scryfall-data.lisp --non-interactive
