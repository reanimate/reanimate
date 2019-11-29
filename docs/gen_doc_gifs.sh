#!/usr/bin/env bash
ROOT=`stack path --project-root`

TINY='^(doc_turbo|doc_viridis|doc_magma|doc_inferno|doc_plasma|doc_sinebow|doc_cividis|doc_jet|
        newline|doc_hsvMatlab|doc_greyscale|doc_parula|doc_rgbComponents|doc_hsvComponents|
        newline|doc_lchComponents|doc_xyzComponents|doc_labComponents)$'

for src in $ROOT/examples/doc_*.hs; do
  BASE=`basename $src .hs`
  DST=$ROOT/docs/gifs/$BASE.gif
  if [[ "$src" -nt "$DST" ]]; then
    echo stack $src render -o $DST
    if [[ "$BASE" =~ $TINY ]]; then
      stack $src render -o $DST --width=320 --height=50
    else
      stack $src render -o $DST
    fi
  fi
done
