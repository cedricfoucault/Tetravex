all: main solve doc clean

main: model view controller
	ocamlopt -I +facile -I +labltk -thread facile.cmxa labltk.cmxa str.cmxa unix.cmxa threads.cmxa tetravexModel.ml tetravexView.ml tetravexController.ml main.ml -o main
	
solve: model
	ocamlopt -I +facile facile.cmxa str.cmxa tetravexModel.ml solve.ml -o solve
	
doc: 
	ocamldoc -html -I +labltk tetravexController.mli tetravexView.mli tetravexModel.mli

controller: view model
	ocamlopt -I +labltk tetravexController.mli
view: model
	ocamlopt -I +labltk tetravexView.mli
model:
	ocamlopt -I +facile tetravexModel.mli

clean:
	rm *.cmi *.cmo *.cmx *.o
	
cleanhtml:
	rm *.html *.css
