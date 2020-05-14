#!/usr/bin/env bash
ROOT=`stack path --project-root`

TINY='^(doc_turbo|doc_viridis|doc_magma|doc_inferno|doc_plasma|doc_sinebow|doc_cividis|doc_jet|
        newline|doc_hsvMatlab|doc_greyscale|doc_parula|doc_rgbComponents|doc_hsvComponents|
        newline|doc_hsv|doc_lchComponents|doc_xyzComponents|doc_labComponents)$'

SQUARE='^(doc_withViewBox)$'

cd $ROOT/examples/
for src in $ROOT/examples/doc_*.hs; do
  BASE=`basename $src .hs`
  DST=$ROOT/docs/gifs/$BASE.gif
  if [[ "$src" -nt "$DST" ]]; then
    if [[ "$BASE" =~ $TINY ]]; then
      echo "stack $src render -o $DST --width=320 --height=50"
      stack $src render -o $DST --width=320 --height=50
    elif [[ "$BASE" =~ $SQUARE ]]; then
      echo "stack $src render -o $DST --width=180 --height=180"
      stack $src render -o $DST --width=180 --height=180
    else
      echo "stack $src render -o $DST"
      stack $src render -o $DST
    fi
  fi
done
