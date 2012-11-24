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

open Parsing;;
# 2 "parser_decls.mly"
	open Printf
	
	open Ast
# 44 "parser_decls.ml"
let yytransl_const = [|
  260 (* LBRACK *);
  261 (* RBRACK *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* LBRACE *);
  265 (* RBRACE *);
  267 (* INTERFACE *);
  268 (* NODE *);
  269 (* FUN *);
  270 (* INTEGER *);
  271 (* CHAR *);
  272 (* FLOAT *);
  273 (* DOUBLE *);
  274 (* STRING *);
  275 (* BOOL *);
  276 (* VOID *);
  283 (* NULL *);
  284 (* BREAK *);
  285 (* CONTINUE *);
  286 (* ELSE *);
  287 (* FOR *);
  288 (* IF *);
  289 (* RETURN *);
  290 (* WHILE *);
  291 (* FORWARD *);
  311 (* QUOTE *);
  312 (* DQUOTE *);
  313 (* NEWLINE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  266 (* ID *);
  278 (* LCONST *);
  279 (* DCONST *);
  280 (* CCONST *);
  281 (* SCONST *);
  282 (* BCONST *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\005\000\005\000\002\000\006\000\006\000\008\000\009\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\008\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\000\000\002\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\002\000\003\000\011\000\000\000\
\007\000\000\000\000\000\000\000\000\000\009\000\008\000\000\000\
\000\000\010\000\000\000\004\000\015\000\013\000\000\000\014\000"

let yydgoto = "\002\000\
\003\000\005\000\006\000\010\000\016\000\019\000\011\000\022\000\
\023\000"

let yysindex = "\002\000\
\000\000\000\000\250\254\255\254\000\000\000\000\000\000\252\254\
\000\000\000\255\006\255\001\255\002\255\000\000\000\000\003\255\
\009\255\000\000\248\254\000\000\000\000\000\000\010\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\014\000\000\000\000\000\000\000\000\000\008\255\
\000\000\000\000\011\255\000\000\000\000\000\000\000\000\007\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yytablesize = 18
let yytable = "\007\000\
\020\000\021\000\001\000\004\000\008\000\009\000\012\000\013\000\
\014\000\007\000\024\000\015\000\017\000\016\000\005\000\012\000\
\018\000\006\000"

let yycheck = "\001\001\
\009\001\010\001\001\000\010\001\006\001\010\001\007\001\002\001\
\008\001\001\001\001\001\010\001\010\001\000\000\007\001\009\001\
\016\000\007\001"

let yynames_const = "\
  LBRACK\000\
  RBRACK\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  INTERFACE\000\
  NODE\000\
  FUN\000\
  INTEGER\000\
  CHAR\000\
  FLOAT\000\
  DOUBLE\000\
  STRING\000\
  BOOL\000\
  VOID\000\
  NULL\000\
  BREAK\000\
  CONTINUE\000\
  ELSE\000\
  FOR\000\
  IF\000\
  RETURN\000\
  WHILE\000\
  FORWARD\000\
  QUOTE\000\
  DQUOTE\000\
  NEWLINE\000\
  EOF\000\
  "

let yynames_block = "\
  ID\000\
  LCONST\000\
  DCONST\000\
  CCONST\000\
  SCONST\000\
  BCONST\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser_decls.mly"
                   ( [], [] )
# 175 "parser_decls.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 74 "parser_decls.mly"
                    ( (_2 :: fst _1), snd _1 )
# 183 "parser_decls.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fun_decl) in
    Obj.repr(
# 75 "parser_decls.mly"
                    ( fst _1, (_2 :: snd _1) )
# 191 "parser_decls.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'arg_decl_list_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'var_decl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'compound_statement) in
    Obj.repr(
# 79 "parser_decls.mly"
    ( { fname		    = _1;
	       formals		    = _3;
	       locals		    = List.rev _6;
	       body		    = List.rev _7 } )
# 204 "parser_decls.ml"
               : 'fun_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser_decls.mly"
              ( [] )
# 210 "parser_decls.ml"
               : 'arg_decl_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_decl_list) in
    Obj.repr(
# 86 "parser_decls.mly"
                    ( List.rev _1 )
# 217 "parser_decls.ml"
               : 'arg_decl_list_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "parser_decls.mly"
                 ( [_1] )
# 224 "parser_decls.ml"
               : 'arg_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_decl_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 90 "parser_decls.mly"
                           ( _3 :: _1 )
# 232 "parser_decls.ml"
               : 'arg_decl_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser_decls.mly"
                ( [] )
# 238 "parser_decls.ml"
               : 'var_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var_decl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_decl) in
    Obj.repr(
# 94 "parser_decls.mly"
                           ( _2 :: _1 )
# 246 "parser_decls.ml"
               : 'var_decl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 97 "parser_decls.mly"
                              ( print_string "declaring variable: "; print_string _1; flush stdout; _1 )
# 253 "parser_decls.ml"
               : 'var_decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 100 "parser_decls.mly"
                ( [] )
# 259 "parser_decls.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'compound_statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 101 "parser_decls.mly"
                                ( _2 :: _1 )
# 267 "parser_decls.ml"
               : 'compound_statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 104 "parser_decls.mly"
              ( Expr(_1) )
# 274 "parser_decls.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser_decls.mly"
        ( Id(_1) )
# 281 "parser_decls.ml"
               : 'expr))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
