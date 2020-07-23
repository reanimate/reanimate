#!/usr/bin/env sh
set -x
stack test --flag reanimate:test --test-arguments="$@" --pedantic
