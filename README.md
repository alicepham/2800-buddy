# 2800-buddy

CS 3110 Fall 2016 :camel:

Professor Michael Clarkson

Final Project

Team Members: Elise Weir, Rohit Curucundhi, Tavish McDonald, Alice Pham

In this project, we attempt to create an animated turing machine using OCAML. We use LablGTK as our GUI package.

##How to run:

To run gui.ml, first install gtk and lablGTK2.

On Mac, 

1. `brew install gtk+ or brew install gtk`
2. `opam install lablgtk`

On Linux, I believe the equivalent for brew is `apt-get`

Then navigate to the director where `gui.ml` is and run:

`ocamlfind ocamlc -g -package lablgtk2 -linkpkg gui.ml -o gui`


