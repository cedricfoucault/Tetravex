tetravex_controller: tetravexview tetravexmodel
	ocamlc -I +facile -I +labltk -thread facile.cma labltk.cma str.cma unix.cma threads.cma tetravex.ml tetravexview.ml tetravex_controller.ml -o main
	
main: view model controller
	ocamlopt -I +facile -I +labltk -thread facile.cmxa labltk.cmxa str.cmxa unix.cmxa threads.cmxa tetravex.ml tetravexview.ml tetravex_controller.ml main.ml -o main

tetravexmodel: tetravexmli
	ocamlc -I +facile facile.cma str.cma tetravex.ml -o tetravex
# tileview: tetravexmli
# 	ocamlc -I +facile -I +lablgtk2 lablgtk.cma gtkInit.cmo facile.cma str.cma tetravex.ml tileview.ml -o tileview
	
tetravexview: ttetravexmli tetravexviewmli
		ocamlc -I +facile -I +labltk labltk.cma facile.cma str.cma unix.cma tetravex.ml tetravexview.ml -o tetravexview

opt: tetravexmliopt
	ocamlopt -I +facile facile.cmxa str.cmxa tetravex.ml -o tetravex

controller: view model
	ocamlopt -I +labltk tetravex_controller.mli
view: model
	ocamlopt -I +labltk tetravexview.mli
model:
	ocamlopt -I +facile tetravex.mli
# gui: guiml
# 	
# guiml:
# 	ocamlc -I +lablgtk2 lablgtk.cma gtkInit.cmo gui.ml -o gui

tetravexviewmli:
	ocamlc -I +labltk tetravexview.mli

tetravexmli:
	ocamlc tetravex.mli

tetravexmliopt:
	ocamlopt tetravex.mli

clean:
	rm *.cmi *.cmo *.cmx *.html *.o
