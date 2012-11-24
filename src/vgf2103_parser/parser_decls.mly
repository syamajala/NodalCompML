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
	%token INTEGER
	%token CHAR
	%token FLOAT
	%token DOUBLE
	%token STRING
	%token BOOL
	%token VOID

	%nonassoc ARRAY

	/*not sure about these*/
	%token <int> LCONST
	%token <float> DCONST
	%token <char> CCONST
	%token <string> SCONST
	%token <bool> BCONST
	
	%token NULL
	
	%token BREAK
	%token CONTINUE
	%token ELSE
	%token FOR
	%token IF
	%token RETURN
	%token WHILE

	%token FORWARD

	%left PLUS
	%left MINUS
	%left TIMES
	%left DIVIDE
	%left MOD
	%left PLUSEQ
	%left MINUSEQ
	%left TIMESEQ
	%left DIVEQ
	%left EQ
	%left NEQ
	%left LT
	%left GT
	%left LEQ
	%left GEQ
	%right NOT
	%left AND
	%left OR
	%left PERIOD
	%token QUOTE
	%token DQUOTE
	
	%token NEWLINE
	%token EOF

	%start program
	%type <Ast.program> program
%%

program:	/*empty*/ { [], [] }
	| program var_decl { ($2 :: fst $1), snd $1 }
	| program fun_decl { fst $1, ($2 :: snd $1) }

fun_decl: /* TODO: break up the stuff inside braces to fun_body */
	ID LPAREN arg_decl_list_opt RPAREN LBRACE var_decl_list compound_statement RBRACE
	   { { fname		    = $1;
	       formals		    = $3;
	       locals		    = List.rev $6;
	       body		    = List.rev $7 } }

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
	/*type_specifier*/ ID SEMI	  { print_string "declaring variable: "; print_string $1; flush stdout; $1 }

compound_statement:
	/*nothing*/		  { [] }
	| compound_statement statement { $2 :: $1 }

statement:
	expr SEMI		  { Expr($1) }

expr:
	ID			  { Id($1) }

/*
type_specifier:
	CHAR			  { $1 }
	| INTEGER		  { int($1) }
	| FLOAT			  { $1 }
	| DOUBLE		  { $1 }
	| STRING		  { $1 }
	| BOOL			  { $1 }
	| VOID			  { $1 }
*/




