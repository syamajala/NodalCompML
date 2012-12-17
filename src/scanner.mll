{ 
  open Parser 
  exception Eof
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let symbol = ['`' '~' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '_' '=' '+' '{' '}' '[' ']' '|' ':' ';' ''' '"' '<' '>' ',' '.' '?' '/']
(*let escape_characters = "\\" | "\\\\" | "\n" | "\t"  (* not sure *) *)
let whitespace = [' ' '\t' '\n']
let identifier = ('_' | letter) ('_' | letter | digit)*
let bool_value = "true" | "false"

rule token = parse
	 whitespace+ { token lexbuf }
	| "//" { comment lexbuf }
	| "node" {NODE}
	| "int" {INT}	(* types and literals *)
	| "char" {CHAR}
	| "float" {FLOAT}
	| "string" {STRING}
	| "bool" {BOOL}
	| "void" {VOID}
	| digit+ as lit {LCONST(int_of_string lit)}
	| (digit*'.'digit+) | (digit+'.'digit*) as lit {DCONST(float_of_string lit)}
	| "\'"(letter | digit | symbol)"\'" as lit {(print_endline "hit single quotes\n"); flush stdout; CCONST(lit.[1])} (* not sure*)
	| "\""(letter | digit | symbol)*"\"" as lit {(print_endline "hit double quotes\n"); flush stdout;SCONST(lit)} (* not sure *)
	| bool_value as lit {BCONST(bool_of_string lit)}
	(*| "null" {NULL} (* we may want this *) *)
	| "break" {BREAK}	(* keywords *)
	| "continue" {CONTINUE}
	| "else" {ELSE}
	| "for" {FOR}
	| "forward" {FORWARD}
	| "fun" {FUN}
	| "if" {IF}
	| "interface" {INTERFACE}
	| "return" {RETURN}
	| "to"	{TO}
	| "while" {WHILE}
	| "print" {PRINT}
	| '(' {LPAREN}	| ')' {RPAREN} 	(*  operators *)
	| '{' {LBRACE}	| '}' {RBRACE}
	| '[' {LBRACK} 	| ']' {RBRACK}
	| '+' {PLUS}	| '-' {MINUS}
	| '*' {TIMES}	| '/' {DIVIDE}
	| '%' {MOD}
	| "+=" {PLUSEQ}	| "-=" {MINUSEQ}
	| "*=" {TIMESEQ}| "/=" {DIVEQ}
	| "==" {EQ}
	| '=' {ASSIGN}
	| "!=" {NEQ}
	| '<' {LT}	| '>' {GT}
	| "<=" {LEQ}	| ">=" {GEQ}
	| '!' {NOT}
	| "&&" {AND}	| "||" {OR}
	| '.' {PERIOD}	| ',' {COMMA}
	| ';' {SEMI}
	| "\'" {QUOTE} 	| "\"" {DQUOTE}
	| identifier as lit {ID(lit)}
	| eof { EOF }
	| _ as char {raise (Failure("illegal character: " 
					^ Char.escaped char))}

	and comment = parse
	  "\r\n" | '\n' { token lexbuf }
	| _ { comment lexbuf }
