type binop = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | And | Or

type unop = Not | Neg | Inc

type expr = 
    Literal of int
  | Id of string
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt = 
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Continue
  | Break
  | Nostmt

type compare = {
  left : expr;
  cmpop : binop;
  right : expr;
}

type fun_decl = {
  return_type : unit;
  fname : string;
  formals : string list; 
  locals : string list;
  body : stmt list;
}

type node = {
	nname : string;
	args : string list;
	local_vars : string list;
	compute : stmt list;
	functions : fun_decl list;
}

type program = node list

let rec string_of_expr = function
  | Literal(l) -> "Num(" ^ string_of_int(l) ^ ")"
  | Id(s) -> "Name('" ^ s ^ "', Load())"
  | Binop(e1, o, e2) ->
      "BinOp(" ^ string_of_expr e1 ^ ", " ^
      (match o with
        Add -> "Add()" | Sub -> "Sub()" | Mult -> "Mult()" | Div -> "Div()" | Mod -> "Mod()"
      | Eq -> "Eq()" | Neq -> "NotEq()"
      | Lt -> "Lt()" | Leq -> "LtE()" | Gt -> "Gt()" | Geq -> "GtE()"
      | Or -> "Or()" | And -> "And()") ^ ", " ^ 
      string_of_expr e2 ^ ")"
  | Unop(o, e1) ->
    "UnaryOp(" ^
      (match o with
        Not -> "Invert()")  ^ string_of_expr e1
  | Assign(v, e) -> "Assign([Name('" ^ v ^ "), Store()], " ^  string_of_expr e ^ ")"
  | Call(f, el) ->
    "Call(" ^ string_of_expr(Id(f)) ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_compare compare = 
  "Compare(" ^ string_of_expr compare.left ^ ", [" ^ 
    (match compare.cmpop with 
      Eq -> "Eq()" | Neq -> "NotEq()" | Lt -> "Lt()" | Leq -> "LtE()" | Gt -> "Gt()" | Geq -> "GtE()")
      ^  "], [" ^ string_of_expr compare.right ^ "])"

let build_compare = function
  | Binop(e1, o, e2) -> { left=e1; cmpop=o; right = e2 }

let rec string_of_stmt = function
    Block(stmts) ->
       String.concat "" (List.map string_of_stmt stmts)
  | Expr(expr) -> string_of_expr expr;
  | Return(expr) -> "Return(" ^ string_of_expr expr ^ ")"
  | If(e, s, Block([])) -> "If(" ^ string_of_compare (build_compare e) ^ ", [" ^ string_of_stmt s ^ "], [])"
  | If(e, s1, s2) ->  "If(" ^ string_of_compare (build_compare e) ^ ", [" ^
      string_of_stmt s1 ^ "], " ^ " [" ^ string_of_stmt s2 ^ "])"
  | For(e1, e2, e3, s) -> string_of_expr e1 ^ string_of_stmt (While(e2, Block([s; Expr(e3)])))
  | While(e, s) -> "While(" ^ string_of_compare (build_compare e) ^ "[" ^ string_of_stmt s ^ "], [])"

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "FunctionDef('" ^ fdecl.fname ^ "', arguments([Name('self', Param())" ^ 
    (if fdecl.formals = [] then "]" else
      (List.fold_left (fun x y -> x ^ ", " ^ y) "" (List.map (fun x -> "Name('" ^ x ^ "', Param())") fdecl.formals) ^ "]")) ^ ", None, None, []), ["
     ^ String.concat "" (List.map string_of_stmt fdecl.body) ^ "], [])]"
(*  String.concat "" (List.map string_of_vdecl fdecl.locals) ^ *)

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

let string_of_node ndecl = 
  "ClassDef('" ^ ndecl.nname ^ "', [], " ^ "[" ^ String.concat ", " (List.map string_of_fdecl ndecl.functions)
