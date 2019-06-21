#!/bin/sh

if [ -d ~/.opam ]; then
  echo "opam already set up, updating just in case"
  opam update
else
  sudo apt install ocamlbuild opam
  opam init
fi

opam install yojson dum
