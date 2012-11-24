LEX=ocamllex
YACC=ocamlyacc
CC=ocamlc

OUTPUTS=ncml *.cmo *.cmi *.mli *~ scanner.ml parser_decls.ml

all: ncml

ncml: parser_decls.cmo ast.cmo scanner.cmo ncml.cmo
	$(CC) -o ncml scanner.cmo parser_decls.cmo ncml.cmo

parser_decls.cmo: parser_decls.ml
	$(CC) -c parser_decls.ml

parser_decls.ml: parser_decls.mli ast.cmo
	$(CC) -c parser_decls.mli

parser_decls.mli: parser_decls.mly
	$(YACC) parser_decls.mly

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


