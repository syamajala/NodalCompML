{ open Parser_decls }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let symbol = ['`' '~' '!' '@' '#' '$' '%' '^' '&' '*' '(' ')' '-' '_' '=' '+' '{' '}' '[' ']' '|' ':' ';' ''' '"' '<' '>' ',' '.' '?' '/']
let escape_characters = "\\" | "\\\\" | "\n" | "\t"  (* not sure *)
let whitespace = [' ' '\t']
let newline = "\r\n" | '\n'
let identifier = letter (letter | digit)*
let bool_value = "true" | "false"

rule token = parse
	 (*whitespace { token lexbuf }*) (* lets use as interpreter for now *)
	  '\n' { NEWLINE }
	| "//" { comment lexbuf }
	| identifier as lit {ID(lit)}
	| "node" {NODE}
	| "int" {INT}	(* types and literals *)
	| "char" {CHAR}
	| "float" {FLOAT}xo
	| "double" {DOUBLE}
	| "string" {STRING}
	| "bool" {BOOL}
	| "void" {VOID}
	| digit+ as lit {INTEGER(int_of_string lit)}
	| (digit*'.'digit+) | (digit+'.'digit*) as lit {DCONST(float_of_string lit)}
	| '''(letter | digit | symbol)''' as lit {CCONST(lit.[1])} (* not sure*)
	| '"'(letter | digit | symbol)*'"' as lit {SCONST(lit)} (* not sure *)
	| bool_value as lit {BCONST(bool_of_string lit)}
	| "null" {NULL} (* we may want this *)
	| "break" {BREAK}	(* keywords *)
	| "continue" {CONTINUE}
	| "else" {ELSE}
	| "for" {FOR}
	| "forward" {FORWARD}
	| "fun" {FUN}
	| "if" {IF}
	| "interface" {INTERFACE}
	| "return" {RETURN}
	| "while" {WHILE}
	| '(' {LPAREN}	| ')' {RPAREN} 	(*  operators *)
	| '{' {LBRACE}	| '}' {RBRACE}
	| '[' {LBRACK} 	| ']' {RBRACK}
	| '+' {PLUS}	| '-' {MINUS}
	| '*' {TIMES}	| '/' {DIVIDE}
	| '%' {MOD}
	| "+=" {PLUSEQ}	| "-=" {MINUSEQ}
	| "*=" {TIMESEQ}| "/=" {DIVEQ}
	| '=' {ASSIGN}
	| "==" {EQ}	| "!=" {NEQ}
	| '<' {LT}	| '>' {GT}
	| "<=" {LEQ}	| ">=" {GEQ}
	| '!' {NOT}
	| "&&" {AND}	| "||" {OR}
	| '.' {PERIOD}	| ',' {COMMA}
	| ';' {SEMI}
	| ''' {QUOTE} 	| '"' {DQUOTE}
	| _ as char {raise (Failure("illegal character: " 
					^ Char.escpared char))}
	| eof {EOF}

	and comment = parse
	  "\r\n" | '\n' { token lexbuf }
	| _ { comment lexbuf }
