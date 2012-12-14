type binop = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
type unop = Not | Neg | Inc

type dtype = CharType | StringType | IntType | FloatType | BoolType | VoidType

type formal = Formal of dtype * string

type expr = 
    Id of string
  | CharLiteral of char
  | StringLiteral of string
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool
  | Binop of expr * binop * expr
  | Unop of unop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type var_decl = VarDecl of dtype * string * expr

type stmt = 
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Print of expr
  | Continue
  | Break
  | Nostmt

type compare = {
  left : expr;
  cmpop : binop;
  right : expr;
}

type fun_decl = {
  return_type : dtype;
  fname : string;
  formals : formal list; 
  locals : var_decl list;
  body : stmt list;
}

(*TODO: rename this to node_decl and change the args/local_vals stuff *)
type node = {
	nname : string;
	args : formal list;
	local_vars : var_decl list;
	compute : stmt list;
	functions : fun_decl list;
}

(*type program = node list*)
type program = node list




let rec string_of_expr = function
    CharLiteral(l) -> "Char(" ^ Char.escaped(l) ^ ")"
  | StringLiteral(l) -> "String(" ^ l ^ ")"
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
  | Print(expr) -> "Print(None, [" ^ string_of_expr expr ^ "], True)"
  | Break -> "Break"
  | Continue -> "Continue"
  | Nostmt -> ""

let string_of_dtype = function
    CharType -> "char"
  | StringType -> "string"
  | IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | VoidType -> "void"

let string_of_vdecl = function
    VarDecl(x, y, z) -> (string_of_dtype x) ^ y ^ (string_of_expr z) ^ ";\n"

let string_of_formal = function
    Formal(x, y) -> (string_of_dtype x) ^ " " ^ y

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formal fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_node ndecl = 
  "ClassDef('" ^ ndecl.nname ^ "', [], " ^ "[" ^ String.concat ", " (List.map string_of_fdecl ndecl.functions) ^ "], [])"

let string_of_program nodes =
  "Module([" ^ (String.concat ", " (List.map string_of_node nodes)) ^ "])"

let n = [{ nname="hi"; args=[]; local_vars=[]; compute=[Print(IntLiteral(1))]; functions=[]}]
