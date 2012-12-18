LEX=ocamllex
YACC=ocamlyacc
CC=ocamlc


OUTPUTS=ncml *.cmo *.cmi *.mli *~ scanner.ml parser.ml *.out *.diff *.log IR.py tests/IR.py tests/IR.py~ *.pyc

all: ncml

ncml: parser.cmo ast.cmo generator.cmo scanner.cmo sast.cmo semantic.cmo ncml.cmo 
	$(CC) -o ncml ast.cmo generator.cmo scanner.cmo parser.cmo sast.cmo semantic.cmo ncml.cmo

semantic_test: semantic_test.cmo
	$(CC) -o semantic_test ast.cmo scanner.cmo parser.cmo sast.cmo semantic.cmo semantic_test.cmo

semantic_test.cmo: ncml semantic_test.ml
	$(CC) -c semantic_test.ml

sast.cmo: sast.ml ast.cmo
	$(CC) -c sast.ml

semantic.cmo: sast.cmo ast.cmo semantic.ml
	$(CC) -c semantic.ml

generator.cmo: generator.ml
	$(CC) -c generator.ml

parser.cmo: parser.ml
	$(CC) -c parser.ml

parser.ml: parser.mli ast.cmo
	$(CC) -c parser.mli

parser.mli: parser.mly
	$(YACC) -v parser.mly

scanner.cmo: scanner.ml
	$(CC) -c scanner.ml

scanner.ml: scanner.mll
	$(LEX) scanner.mll

ncml.cmo: ncml.ml
	$(CC) -c ncml.ml

ast.cmo: ast.ml
	$(CC) -c ast.ml

clean:
	rm -rf $(OUTPUTS)


