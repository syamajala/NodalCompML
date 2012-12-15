type binop = Add | Sub | Mult | Div | Mod | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
type unop = Not | Neg

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

type boolop = {
  bop : binop;
  left : expr;
  right : expr;
}

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

type program = node list

let rec string_of_expr = function
    CharLiteral(l) -> "Str(" ^ Char.escaped(l) ^ ")"
  | StringLiteral(l) -> "Str(" ^ l ^ ")"
  | IntLiteral(l) -> "Num(" ^ string_of_int(l) ^ ")"
  | FloatLiteral(l) -> "Num(" ^ string_of_float(l) ^ ")"
  | BoolLiteral(l) -> "Bool(" ^ string_of_bool(l) ^ ")"
  | Id(s) -> "Name('" ^ s ^ "', Load())"
  | Binop(e1, o, e2) -> 
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
        Not -> "Not()"
        | Neg -> "USub()")  ^ string_of_expr e1 ^ ")"
  | Assign(v, e) -> "Assign([Name('" ^ v ^ "'), Store()], " ^  string_of_expr e ^ ")"
  | Call(f, el) ->
    "Call(Attribute(Name('self', Load()), '" ^ f ^ "', Load())" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

(*
let string_of_boolop boolop =
  "BoolOp(" ^ 
    (match boolop.bop with
        And -> "And()"
      | Or -> "Or()") ^ ", [" ^ string_of_expr boolop.left ^ ", " ^ string_of_expr boolop.right ^ "])"
*)

let string_of_compare compare = 
  "Compare(" ^ string_of_expr compare.left ^ ", [" ^ 
    (match compare.cmpop with 
        Eq -> "Eq()" | Neq -> "NotEq()" 
      | Lt -> "Lt()" | Leq -> "LtE()" 
      | Gt -> "Gt()" | Geq -> "GtE()" )
      ^  "], [" ^ string_of_expr compare.right ^ "])"

let build_compare = function
  | Binop(e1, o, e2) -> { left=e1; cmpop=o; right = e2 }

(*
let build_boolop = function
  | Binop(e1, o, e2) -> { boolop=o; left=e1; left=e2 }
*)

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
    VarDecl(x, y, z) -> string_of_expr (Assign(y,z))

let string_of_formal = function
    Formal(x, y) -> y

let string_of_fdecl fdecl =
  "FunctionDef('" ^ fdecl.fname ^ "', arguments([Name('self', Param())" ^ 
    (if fdecl.formals = [] then "]" else
      (List.fold_left (fun x y -> x ^ ", " ^ y) "" (List.map (fun x -> "Name('" ^ string_of_formal x ^ "', Param())") fdecl.formals) ^ "]")) ^ ", None, None, []), [" 
     ^ (String.concat ", " (List.map string_of_vdecl fdecl.locals)) ^ " " 
     ^ (String.concat " " (List.map string_of_stmt fdecl.body)) ^ "], [])]"

let string_of_compute c = 
  string_of_stmt (Block(c.compute))

let string_of_node ndecl = 
  "ClassDef('" ^ ndecl.nname ^ "', [], " ^ "[" ^ String.concat ", " (List.map string_of_fdecl ndecl.functions) ^ "], [])"

let string_of_program nodes =
  "Module([" ^ (String.concat ", " (List.map string_of_node nodes)) ^ "])"

let v = VarDecl(StringType, "x", StringLiteral("mesg"))
let f = { return_type = VoidType; fname = "sayhi"; formals = []; locals = []; body = [Print(StringLiteral("hi"))] }
let n = [{ nname="hi"; args=[]; local_vars=[]; compute=[Expr(Call("sayhi", [Noexpr]))]; functions=[f]}]


(*
"""class hi():\n    def compute(self):\n        self.sayhi()\n\n    def sayhi(self):\n        print 'hi'"""

class hi():
    def compute(self):
        self.sayhi()

    def sayhi(self):
        print 'hi'

"Module([ClassDef('hi', [], 
  [FunctionDef('compute', arguments([Name('self', Param())], None, None, []), [Expr(Call(Attribute(Name('self', Load()), 'sayhi', Load()), [], [], None, None))], []), 
  FunctionDef('sayhi', arguments([Name('self', Param())], None, None, []), [Print(None, [Str('hi')], True)], [])], [])])"
*)

(*
"""class hi():\n    def compute(self):\n        self.sayhi('hi')\n\n    def sayhi(self, mesg):\n        print mesg"""

class hi():
    def compute(self):
        self.sayhi('hi')

    def sayhi(self, mesg):
        print mesg

hi().compute()

"Module([ClassDef('hi', [], [FunctionDef('compute', arguments([Name('self', Param())], None, None, []), [Expr(Call(Attribute(Name('self', Load()), 'sayhi', Load()), [Str('hi')], [], None, None))], []), FunctionDef('sayhi', arguments([Name('self', Param()), Name('mesg', Param())], None, None, []), [Print(None, [Name('mesg', Load())], True)], [])], [])])"
*)
