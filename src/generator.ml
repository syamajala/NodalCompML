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

let rec str_of_stmt = function
  | Block(stmts) -> (String.concat "\t\t " (List.map str_of_stmt stmts))
  | Expr(expr) -> str_of_expr expr;
  | Return(expr) -> "return " ^ str_of_expr expr
  | If(e, s, _) -> "if (" ^ str_of_expr e ^ "):\n\t\t\t " ^ str_of_stmt s
  | If(e, s1, s2) -> "if (" ^ str_of_expr e ^ "):\n\t\t\t " ^ str_of_stmt s1 ^ "\n\t\t else:\n\t\t\t " ^ str_of_stmt s2
  | For(e1, e2, e3, s) -> str_of_expr e1 ^ "\n\t\t " ^ "while(" ^ str_of_expr e2 ^ "):\n\t\t\t " ^ str_of_stmt s ^ "\n\t\t\t " ^ str_of_expr e3
  | While(e, s) -> "<<while>>"
  | Print(expr) -> "print " ^ (str_of_expr expr)
  | Break -> "break"
  | Continue -> "continue"
  | Nostmt -> ""

let str_of_vdecl = function
  | VarDecl(t, v, e) -> str_of_expr (Assign(v, e))

let str_of_formal = function
  | Formal(_, v) -> v

let str_of_fdecl fdecl =
"\t def " ^ fdecl.fname ^ "(self, " ^ (String.concat ", " (List.map str_of_formal fdecl.formals)) ^ "):\n"
^
"\t\t " ^ (String.concat "\n\t\t " (List.map str_of_vdecl fdecl.locals)) ^ "\n"
^
"\t\t " ^ (String.concat "\n\t\t " (List.map str_of_stmt fdecl.body))

let str_of_compute ndecl = 
  str_of_fdecl ({ return_type = VoidType; fname = "compute"; formals = ndecl.args; locals = []; body = ndecl.compute })

let str_of_node ndecl =
"class " ^ ndecl.nname ^ "(): # node name\n"
^
"\t " ^ (String.concat "\n\t " (List.map str_of_vdecl ndecl.local_vars)) ^ "\n"
^
(String.concat "\n" (List.map str_of_fdecl ndecl.functions)) ^ "\n"
^
(str_of_compute ndecl)


let str_of_program nodes =
let res = 
"def forward(node, args):
\t nodes[node].compute(*args)

def main():
\t global nodes
\t nodes = { 'start': start(), 'node1': node1(), 'node2': node2() }
# kick off the calculation by calling all start nodes' compute functions.
# using one start node for now...
\t nodes['start'].compute()

"
^
(String.concat "\n" (List.map str_of_node nodes))
^
"
class node1():
\t def compute(self):
\t\t forward('node2', [\"hi\"])

class node2():
\t def compute(self, arg1):
\t\t print arg1


main()
" in
print_endline res; res
