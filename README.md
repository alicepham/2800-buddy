# 2800-buddy

CS 3110 Fall 2016 :camel:

Professor Michael Clarkson

Final Project

Team Members: Elise Weir, Rohit Curucundhi, Tavish McDonald, Alice Pham

In this project, we attempt to create an animated turing machine using OCAML. We use LablGTK2 as our GUI package of choice.

##How to run Turing machine visualization:

To run Turing machine visualization, first install `gtk` and `lablGTK2`.

On Mac, 

1. `brew install gtk+` or `brew install gtk`
2. `opam install lablgtk`

On Linux, I believe the equivalent for brew is `apt-get`

Then navigate to the directory where `gui.ml` is and run:

1. `ocamlfind ocamlc -g -package lablgtk2 -linkpkg gui.ml -o gui`
2. `./gui`



