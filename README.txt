------------------------------------------------------------------------------
---------------------------------- TETRAVEX ----------------------------------
------------------------------------------------------------------------------

$> make main
To compile the main application (./main to execute).

$> make solve
To compile the solver program
(reads on standard input, prints on standard output).

$> make test
To compile the test script:
Generate a well-formed puzzle randomly, print it on standard output,
solve it, print the solution on standard output
(expects m = number of line, n =  number of columns, c = number of colors 
 given as arguments)

$> make doc
To generate the documentation with ocamldoc.

$> make clean
To clean the precompiled junk (.cmi, .cmx, .cmo).

$> make all
To do the 5 steps above all at once.

You need to have the standard ocaml library and FaCiLe 
(Functional Constraint Library - see http://www.recherche.enac.fr/opti/facile/)
to compile this program on your machine.

Template of a puzzle instance:

number_of_lines number_of_columns number_of_colors
left top right bottom // (line, column) = (1, 1)
left top right bottom // (line, column) = (1, 2)
...
left top right bottom // (line, column) = (1, n)
left top right bottom // (line, column) = (2, 1)
...
left top right bottom // (line, column) = (m, n)
