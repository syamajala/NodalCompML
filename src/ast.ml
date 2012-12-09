type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | Not | And | Or

type expr = 
    Literal of int
  | Id of string
  | Binop of expr * op * expr
  | Unop of op * expr
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
      | Lt -> "Lt()" | Leq -> "LtE()" | Gt -> "Gt()" | Geq -> "GtE()") ^ ", " ^ 
      string_of_expr e2 ^ ")"
  | Assign(v, e) -> "[Name('" ^ v ^ "), Store())]" ^  string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "Return(" ^ string_of_expr expr ^ ")"
  | If(e, s, Block([])) -> "If(" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "If(" ^ string_of_expr e ^ "," ^ "[" ^
      string_of_stmt s1 ^ "], " ^ ",[" ^ string_of_stmt s2 ^ "])"
  | For(e1, e2, e3, s) ->
      "For(" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "While(" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
