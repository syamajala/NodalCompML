(*
Notes: tab convention is one tab and a space: "\t "

TODO:
  - Make tabbing of statements be +1 the enclosing scope's tab count. 
    Try passing this count along to every str_of func.
  - BETTER IDEA: instead of a count, pass along a string of tabs, append to this
    string of tabs before passing it to a function that writes the next level. "\t\t" --> "\t\t\t"
*)

open Ast
open Printf

let tab lvl =
  String.make lvl '\t'

let str_of_bool = function
  | true -> "True"
  | false -> "False"

let rec str_of_expr = function
  | CharLiteral(l) -> Char.escaped(l)
  | StringLiteral(l) -> l
  | IntLiteral(l) -> string_of_int(l)
  | FloatLiteral(l) -> string_of_float(l)
  | BoolLiteral(l) -> str_of_bool(l)
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    str_of_expr e1 ^ " " ^
      (match o with
	  Add -> "+" | Sub -> "-" 
	| Mult -> "*" | Div -> "/" | Mod -> "%"
	| Eq -> "==" | Neq -> "!="
	| Lt -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">="
	| Or -> "or" | And -> "and" )
    ^ " " ^ str_of_expr e2
  | Unop(o, e) ->
    (match o with
	Not -> "not" | Neg -> "-") ^ str_of_expr e
  | Assign(v, e) -> v ^ " = " ^ (str_of_expr e)
  | Call(f, e) -> "self." ^ f ^ "(" ^ (String.concat ", " (List.map str_of_expr e)) ^ ")"
  | Noexpr -> ""

let rec str_of_stmt s lvl =
  match s with
  | Block(stmts) -> (String.concat "\n" (List.map (fun x-> str_of_stmt x (lvl+1)) stmts))
  | Expr(expr) -> str_of_expr expr;
  | Return(expr) -> "return " ^ str_of_expr expr
  | If(e, s, _) -> "if (" ^ str_of_expr e ^ "):\n" ^ (tab (lvl+1)) ^ str_of_stmt s (lvl+1)
  | If(e, s1, s2) -> "if (" ^ str_of_expr e ^ "):\n" ^ (tab (lvl+1)) ^ str_of_stmt s1 (lvl+1) ^ "\n" ^ (tab lvl) ^ "else:\n" ^ (tab (lvl+1)) ^ str_of_stmt s2 (lvl+1)
  | For(e1, e2, e3, s) -> str_of_expr e1 ^ "\n" ^ (tab lvl) ^ "while(" ^ str_of_expr e2 ^ "):\n" ^ (tab (lvl+1)) ^ str_of_stmt s (lvl+1) ^ "\n" ^ (tab (lvl+1)) ^ str_of_expr e3
  | While(e, s) -> "\n" ^ (tab lvl) ^ "while(" ^ str_of_expr e ^ "):\n" ^ (tab (lvl+1)) ^ str_of_stmt s (lvl+1)
  | Forward(elist, dest) -> "\n" ^ (tab lvl) ^ "forward('" ^ dest ^ "', [" ^ (String.concat "," (List.map str_of_expr elist)) ^ "])"
  | Print(expr) -> "print " ^ (str_of_expr expr)
  | Break -> "break"
  | Continue -> "continue"
  | Nostmt -> ""

let str_of_vdecl v lvl = 
  match v with
  | VarDecl(t, v, e) -> (tab lvl) ^ (str_of_expr (Assign(v, e)))

let str_of_formal = function
  | Formal(_, v) -> v

let str_of_fdecl fdecl lvl =
  (tab lvl) ^ "def " ^ fdecl.fname ^ "(self" ^ 
  (if fdecl.formals = [] then 
      "" else
      ", " ^ (String.concat ", " (List.map str_of_formal fdecl.formals))) 
  ^ "):\n"
  ^ (String.concat "\n" (List.map (fun x-> str_of_vdecl x (lvl+1)) fdecl.locals)) 
  ^ "\n" ^ (tab (lvl+1)) ^ 
    (let l = "\n"^(tab (lvl+1)) in
    (String.concat l (List.map (fun x-> str_of_stmt x (lvl+1)) fdecl.body)))

let str_of_compute ndecl = 
  str_of_fdecl ({ return_type = VoidType; fname = "compute"; formals = ndecl.args; locals = ndecl.local_vars; body = ndecl.compute }) 1

let str_of_node ndecl =
"class " ^ ndecl.nname ^ "(): # node name\n"
 ^ (String.concat "\n" (List.map (fun x-> str_of_vdecl x 1) ndecl.local_vars)) ^ "\n" 
  ^ (String.concat "\n" (List.map (fun x-> str_of_fdecl x 1) ndecl.functions)) ^ "\n\n"
^ (str_of_compute ndecl)

let node_key_value ndecl =
  "'" ^ ndecl.nname ^ "':" ^ ndecl.nname ^ "()"

let str_of_program nodes =
let res = 
"def forward(node, args):
\t nodes[node].compute(*args)

def main():
\t global nodes
\t nodes = { "
^ 
(String.concat ", " (List.map node_key_value nodes))
^" }
# kick off the calculation by calling all start nodes' compute functions.
# using one start node for now...
\t nodes['start'].compute()

"
^
(String.concat "\n\n" (List.map str_of_node nodes))
^
"

main()
" in
(*print_endline res;*) res
