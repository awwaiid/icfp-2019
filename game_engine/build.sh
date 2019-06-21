#!/bin/sh

ocamlbuild -r -use-ocamlfind -pkgs yojson,bigarray,dum engine.native

