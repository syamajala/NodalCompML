type binop = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
type unop = Not | Neg | Inc

type dtype = CharType | StringType | IntType | FloatType | BoolType | VoidType

type formal = Formal of dtype * string

type expr = 
    Id of string
  | CharLiteral of char
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type var_decl  = VarDecl of dtype * string * expr

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
  return_type : dtype;
  fname : string;
  formals : string list; 
  locals : string list;
  body : stmt list;
}

(*TODO: rename this to node_decl and change the args/local_vals stuff *)
type node = {
	nname : string;
	args : string list;
	local_vars : string list;
	compute : stmt list;
	functions : fun_decl list;
}

type program = node list




let rec string_of_expr = function
    CharLiteral(l) -> "Char(" ^ Char.escaped(l) ^ ")"
  | IntLiteral(l) -> "Int(" ^ string_of_int(l) ^ ")"
  | FloatLiteral(l) -> "Float(" ^ string_of_float(l) ^ ")"
  | BoolLiteral(l) -> "Bool(" ^ string_of_bool(l) ^ ")"
  | Id(s) -> "Name('" ^ s ^ "', Load())"
  | Binop(e1, o, e2) -> (*TODO: why do we have the "()"?? can probably get rid of them*)
      "BinOp(" ^ string_of_expr e1 ^ ", " ^
      (match o with
        Add -> "Add()" | Sub -> "Sub()" | Mult -> "Mult()" | Div -> "Div()" | Mod -> "Mod()"
      | Eq -> "Eq()" | Neq -> "NotEq()"
      | Lt -> "Lt()" | Leq -> "LtE()" | Gt -> "Gt()" | Geq -> "GtE()"
      | Or -> "Or()" | And -> "And()" ) ^ ", " ^ 
      string_of_expr e2 ^ ")"
  | Unop(o, e1) ->
      "UnOp(" ^
      (match o with
        Not -> "Not" | Inc -> "Inc" | Neg -> "Neg") ^ ", " ^ string_of_expr e1 ^ ")"
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
  | Break -> "Break;"
  | Continue -> "Continue;"
  | Nostmt -> ""

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
