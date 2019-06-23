#!/bin/sh

ocamlbuild -r -use-ocamlfind -pkgs yojson,dum,str engine.native

