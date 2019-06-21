#!/bin/sh

ocamlbuild -r -use-ocamlfind -pkgs yojson,dum engine.native

