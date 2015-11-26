all:
	ocamlopt -o ast ast.ml
	menhir --infer parser.mly
	ocamllex lexer.mll
	rm parser.mli
	ocamlopt -o main  parser.ml lexer.ml main.ml


	for f in exec/*.scala; do ./main $$f; done 

clean :
	rm -rf main *.cmo *.mli *.cmi *.cmx *~ *.o
	rm parser.ml lexer.ml
	rm a.out ast
