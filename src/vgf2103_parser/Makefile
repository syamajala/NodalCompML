LEX=ocamllex
YACC=ocamlyacc
CC=ocamlc

OUTPUTS=ncml *.cmo *.cmi *.mli *~ lexer.ml parser_decls.ml

all: ncml

ncml: parser_decls.cmo lexer.cmo ncml.cmo
	$(CC) -o ncml lexer.cmo parser_decls.cmo ncml.cmo

parser_decls.cmo: parser_decls.ml
	$(CC) -c parser_decls.ml

parser_decls.ml: parser_decls.mli
	$(CC) -c parser_decls.mli

parser_decls.mli:
	$(YACC) parser_decls.mly

lexer.cmo: lexer.ml
	$(CC) -c lexer.ml

lexer.ml: lexer.mll
	$(LEX) lexer.mll

ncml.cmo:
	$(CC) -c ncml.ml

clean:
	rm -rf $(OUTPUTS)


