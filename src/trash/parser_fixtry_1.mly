%{
	open Printf
	
	open Ast
%}
%token <string> ID

%token INT
%token CHAR
%token FLOAT
%token DOUBLE /* should handle this differently, or remove doubles */
%token STRING
%token BOOL
%token VOID /* an indicator, can't actually have a void "thing" */

  /*TODO: rename LCONST --> ICONST, DCONST --> FCONST*/
%token <int> LCONST
%token <float> DCONST
%token <char> CCONST
%token <string> SCONST
%token <bool> BCONST

%left PLUS MINUS TIMES DIVIDE MOD
%left EQ NEQ LT GT LEQ GEQ AND OR
%right NOT

%right ASSIGN
%token SEMI

%token NEWLINE
%token EOF

%start program
%type <Ast.program> program
%%

program: 
	 /* empty */	        { [] }
	| program var_decl	{ $2 :: $1 }


var_decl:
	dtype ID ASSIGN expr SEMI	  { print_string "declaring variable: "; print_string $2; flush stdout; $2 }

expr:
	| ID			  { Id($1) }
	| CCONST		  { CharLiteral($1) }
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

