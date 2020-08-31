#!/usr/bin/env sh
set -x
stack test --test-arguments="--rerun -j 4 $@" --pedantic
