#!/usr/bin/env bash
ROOT=`stack path --project-root`

for src in $ROOT/examples/doc_*.hs; do
  BASE=`basename $src .hs`
  DST=$ROOT/docs/gifs/$BASE.gif
  if [[ "$src" -nt "$DST" ]]; then
    echo stack $src render -o $DST
    stack $src render -o $DST
  fi
done
