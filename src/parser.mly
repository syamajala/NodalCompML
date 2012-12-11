%{
	open Printf
	
	open Ast
%}

	%left SEMI COMMA
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
	%token PLUSEQ MINUSEQ TIMESEQ DIVEQ
	%token ASSIGN
	%token EQ NEQ LT GT LEQ GEQ
	%token AND OR NOT
	%token PERIOD COMMA SEMI
	%token QUOTE DQUOTE

	%token FORWARD
	%token TO

	%token NEWLINE
	%token EOF

	%start program
	%type <Ast.program> program
%%

program: 
	| /* empty */	{ [] }
	| program node	{ $2 :: $1 }

node:
	NODE ID LPAREN arg_decl_list_opt RPAREN 
		LBRACE  var_decl_list
			compound_statement
			fun_decl_list	RBRACE 
	{ { nname 	= $2;
	    args 	= $4;
	    local_vars 	= List.rev $7;
   	    compute 	= List.rev $8;
	    functions 	= List.rev $9 } }

fun_decl_list :
	| /* empty */			{ [] }
	| fun_decl_list fun_decl	{ $2 :: $1 }

fun_decl: /* TODO: break up the stuff inside braces to fun_body */
	dtype FUN ID LPAREN arg_decl_list_opt RPAREN 
	   LBRACE var_decl_list 
		  compound_statement RBRACE
	{ { return_type = $1;
	    fname	= $3;
	    formals	= $5;
	    locals	= List.rev $8;
	    body	= List.rev $9 } }

arg_decl_list_opt:	
	/*empty*/		  { [] }
	| arg_decl_list		  { List.rev $1 }

arg_decl_list:
	  ID		       	  { [$1] }
	| arg_decl_list COMMA ID  { $3 :: $1 }

var_decl_list:
	/*nothing*/		  { [] }
	| var_decl_list var_decl  { $2 :: $1 }

var_decl:
	dtype ID ASSIGN expr SEMI	  { print_string "declaring variable: "; print_string $2; flush stdout; $2 }

compound_statement:
	/*nothing*/		  { [] }
	| compound_statement stmt { $2 :: $1 }

stmt:
	LBRACE compound_statement RBRACE  { Block($2) }
	| expr SEMI		  		{ Expr($1) }
	| IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Nostmt) }
	| IF LPAREN expr RPAREN stmt ELSE  stmt   { If($3, $5, $7) }
	| WHILE LPAREN expr RPAREN stmt		{ While($3, $5) }
	| FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt{ For($3, $5, $7, $9) }	| BREAK SEMI	{ Break } 
	| CONTINUE SEMI	{ Continue }
	| RETURN expr SEMI	{ Return($2) }
	| RETURN SEMI		{ Return(Noexpr) }
	/*FORWARD */

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

