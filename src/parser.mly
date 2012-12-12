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
%token NOELSE
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
	  | program var_decl NEWLINE { $2 :: $1 }
/*	  | program error NEWLINE { print_string("error found...");[] } */


/*print_string "declaring variable: "; print_string $2; flush stdout; $2*/

var_decl:
           dtype ID ASSIGN expr SEMI      { print_string("var declared..."); flush stdout;VarDecl($1, $2, $4) }
	/*dtype ID ASSIGN expr SEMI	  { VarDecl($1, $2, $4) }*/

expr:
	  ID			  { Id($1) }
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
	| NOT expr 		  { Unop(Not, $2) }

dtype:
	  CHAR	 { CharType }
	| STRING { StringType }
	| INT	 { IntType }
	| FLOAT  { FloatType }
	| BOOL   { BoolType }
	| VOID   { VoidType }

