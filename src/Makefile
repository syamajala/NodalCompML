LEX=ocamllex
YACC=ocamlyacc
CC=ocamlc


OUTPUTS=ncml *.cmo *.cmi *.mli *~ scanner.ml parser.ml *.out *.diff *.log IR.py tests/IR.py tests/IR.py~ *.pyc

all: ncml

ncml: parser.cmo ast.cmo generator.cmo scanner.cmo ncml.cmo
	$(CC) -o ncml ast.cmo generator.cmo scanner.cmo parser.cmo ncml.cmo

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


