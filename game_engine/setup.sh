#!/bin/sh

if [ -d ~/.opam ]; then
  echo "opam already set up"
else
  sudo apt install opam
  opam init
fi

opam install yojson dum
