type token =
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | ID of (string)
  | INTERFACE
  | NODE
  | FUN
  | INTEGER
  | CHAR
  | FLOAT
  | DOUBLE
  | STRING
  | BOOL
  | VOID
  | LCONST of (int)
  | DCONST of (float)
  | CCONST of (char)
  | SCONST of (string)
  | BCONST of (bool)
  | NULL
  | BREAK
  | CONTINUE
  | ELSE
  | FOR
  | IF
  | RETURN
  | WHILE
  | FORWARD
  | QUOTE
  | DQUOTE
  | NEWLINE
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
