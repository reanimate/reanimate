#!/usr/bin/env sh
set -x
stack test --flag reanimate:test --test-arguments="--rerun -j 4 $@" --pedantic
