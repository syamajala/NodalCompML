
(* file: lexer.mll *)
{
  open Parser_decls
}
let digit = ['0'-'9']
rule token = parse
[' ' '\t' '\r']    { token lexbuf }
| [ '\n' ] 
{
    lineno := !lineno + 1;
    incr_linenum lexbuf;
    token lexbuf
}
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRACE }
| '}' { RBRACE }
| ';' { SEMI }
| ',' { COMMA }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { ASSIGN }
| "==" { EQ }
| "!=" { NEQ }
| '<' { LT }
| "<=" { LEQ }
| '>' { GT }
| '.' { DOT }
| ">=" { GEQ }
| "fun" { FUN }
| "node" { NODE }
| "if" { IF }
| "else" { ELSE }
| "while" { WHILE }
| "for" { FOR }
| "break" { BREAK }
| "forward" { FORWARD }
| "bool" { BOOL }
| "true" { TRUE }
| "false" { FALSE }
| "int" { INT }
| "float" { FLOAT }
| "char" { CHAR }
| (['1'-'9']['0'-'9']* | '0') as lxm 
{
    try 
        INTV(int_of_string lxm) 
    with e -> 
        let errormsg = "Syntax Error: Integer '" ^ lxm ^ "' overflow at line: " ^ string_of_int !lineno in
        Printf.printf "%s\n"  errormsg;
        INTV(0)
}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char
{
    let line = linecounter lexbuf in
    let col = columnstart lexbuf in
    raise ( IllegalCharacterError(Char.escaped char, line, col) );
}

