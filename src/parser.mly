%{
	open Printf
	
	open Ast

	let parse_error e = print_endline e; flush stdout
%}

%token SEMI COMMA PERIOD
%token ASSIGN
%right ASSIGN
%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE
%token <string> ID

%token INTERFACE
%token NODE
%token FUN
%token INT
%token CHAR
%token FLOAT
%token DOUBLE /* should handle this differently, or remove doubles */
%token STRING
%token BOOL
%token VOID /* an indicator, can't actually have a void "thing" */

%nonassoc ARRAY

/*TODO: rename LCONST --> ICONST, DCONST --> FCONST*/
%token <int> LCONST
%token <float> DCONST
%token <char> CCONST
%token <string> SCONST
%token <bool> BCONST

%token BREAK
%token CONTINUE
%token ELSE
%nonassoc ELSE
%nonassoc NOELSE
%token FOR
%token IF
%token RETURN
%token WHILE

%token PLUS MINUS TIMES DIVIDE MOD
%left PLUS MINUS TIMES DIVIDE MOD
%token PLUSEQ MINUSEQ TIMESEQ DIVEQ
%right PLUSEQ MINUSEQ TIMESEQ DIVEQ
%token EQ NEQ LT GT LEQ GEQ
%left EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%left AND OR NOT
%token QUOTE DQUOTE

%token FORWARD
%token TO

%token NEWLINE
%token EOF

%start program
%type <Ast.program> program

%%

program: 
          | /*empty*/	   { [] }
          | program NEWLINE  { [] }
	  | program node NEWLINE { $2 :: $1 }
/*	  | program error NEWLINE { print_string("error found...");[] } */

node: /*think of a better way to handle these 3 optional things in a row...*/
        NODE ID LPAREN formal_list_opt RPAREN 
                LBRACE  var_decl_list NEWLINE
	                fun_decl_list NEWLINE
                        compound_statement RBRACE
        { { nname       = $2;
            args        = $4;
            local_vars  = List.rev $7;
            functions   = List.rev $9;
            compute     = List.rev $11 } }

fun_decl_list :
          | /* empty */                   { [] }
          | fun_decl_list fun_decl        { $2 :: $1 }

fun_decl:
        dtype FUN ID LPAREN formal_list_opt RPAREN
           LBRACE var_decl_list
                  compound_statement RBRACE
        { { return_type = $1;
            fname       = $3;
            formals     = $5;
            locals      = List.rev $8;
            body        = List.rev $9 } }

compound_statement:
	    /*nothing*/           { [] }
	  | compound_statement stmt { $2 :: $1 }

stmt:
            expr SEMI                             { Expr($1) }
          | LBRACE compound_statement RBRACE  { Block($2) }
          | IF LPAREN expr RPAREN stmt %prec NOELSE { print_endline("IF STATMENT HIT ELSELESS"); flush stdout;If($3, $5, Nostmt) }
          | IF LPAREN expr RPAREN stmt ELSE stmt   { print_endline("IF ELSE HIT"); flush stdout; If($3, $5, $7) }
          | WHILE LPAREN expr RPAREN stmt         { While($3, $5) }
          | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt  { For($3, $5, $7, $9) }
	  | BREAK SEMI    { Break }
          | CONTINUE SEMI { Continue }
          | RETURN expr SEMI      { Return($2) }
          | RETURN SEMI           { Return(Noexpr) }
	    /*FORWARD*/

formal_list_opt:
        /*empty*/                 { [] }
          | formal_list           { List.rev $1 }

formal_list:
          dtype ID                      { [Formal($1, $2)] }
          | formal_list COMMA dtype ID  { Formal($3, $4) :: $1 }

actual_list_opt:
	    /*empty*/             { [] }
	  | actual_list           { List.rev $1 }

actual_list:
	    expr             { [$1] }
	  | actual_list COMMA expr { $3 :: $1 }

var_decl_list:
          /* empty */                { [] }
	  | var_decl_list var_decl { $2 :: $1 }

var_decl:
           dtype ID ASSIGN expr SEMI      { print_string("var declared..."); flush stdout; VarDecl($1, $2, $4) }

expr:
	  ID			  { Id($1) }
	| ID ASSIGN expr          { Assign($1, $3) }
	| CCONST		  { CharLiteral($1) }
	| SCONST                  { StringLiteral($1) }
	| LCONST		  { IntLiteral($1) }
	| DCONST		  { FloatLiteral($1) }
	| BCONST		  { BoolLiteral($1) }
	| expr PLUS expr	  { Binop($1, Add, $3) }
	| expr MINUS expr	  { Binop($1, Sub, $3) }
	| expr TIMES expr	  { Binop($1, Mult, $3) }
	| expr DIVIDE expr	  { Binop($1, Div, $3) }
	| expr MOD expr	  	  { Binop($1, Mod, $3) }
/*	| ID PLUSEQ expr	  { Binop($1, Pluseq, $3) } */
	| expr EQ expr		  { Binop($1, Eq, $3) }
	| expr NEQ expr		  { Binop($1, Neq, $3) }
	| expr LT expr		  { Binop($1, Lt, $3) }
	| expr GT expr		  { Binop($1, Gt, $3) }
	| expr LEQ expr		  { Binop($1, Leq, $3) }
	| expr GEQ expr		  { Binop($1, Geq, $3) }
	| expr AND expr		  { Binop($1, And, $3) }
	| expr OR expr		  { Binop($1, Or, $3) }
	| ID LPAREN actual_list_opt RPAREN { Call($1, $3) }
	| LPAREN expr RPAREN      { $2 }
	| NOT expr 		  { Unop(Not, $2) }

dtype:
	  CHAR	 { CharType }
	| STRING { StringType }
	| INT	 { IntType }
	| FLOAT  { FloatType }
	| BOOL   { BoolType }
	| VOID   { VoidType }

