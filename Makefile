all: tetravexmli
	ocamlc -I +facile facile.cma str.cma tetravex.ml -o tetravex
	
tile_view: tetravexmli
	ocamlc -I +facile -I +lablgtk2 lablgtk.cma gtkInit.cmo facile.cma str.cma tetravex.ml tile_view.ml -o tile_view

opt: tetravexmliopt
	ocamlopt -I +facile facile.cmxa str.cmxa tetravex.ml -o tetravex

gui: guiml
	
guiml:
	ocamlc -I +lablgtk2 lablgtk.cma gtkInit.cmo gui.ml -o gui

tetravexmli:
	ocamlc tetravex.mli

tetravexmliopt:
	ocamlopt tetravex.mli

clean:
	rm *.cmi *.cmo *.cmx *.html
