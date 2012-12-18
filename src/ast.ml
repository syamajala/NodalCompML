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
  | Forward of expr list * string
  | Print of expr
  | Continue
  | Break
  | Nostmt

type boolop = {
  left : expr;
  bop : binop;
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


(*
let attributesref = ref []

let rec string_of_expr = function
    CharLiteral(l) -> "Str('" ^ Char.escaped(l) ^ "')"
  | StringLiteral(l) -> "Str('" ^ l ^ "')"
  | IntLiteral(l) -> "Num(" ^ string_of_int(l) ^ ")"
  | FloatLiteral(l) -> "Num(" ^ string_of_float(l) ^ ")"
  | BoolLiteral(l) -> "Bool(" ^ string_of_bool(l) ^ ")"
  | Id(s) -> (if List.mem s !attributesref then
      "Attribute(Name('self', Load()), '" else
      "Name('") ^ s ^ "', Load())"
  | Binop(e1, o, e2) ->
      (match o with
      |Add|Sub|Mult|Div|Mod -> "BinOp(" ^ string_of_expr e1 ^ ", " ^ 
        (match o with
        | Add -> "Add()"
        | Sub -> "Sub()"
        | Mult -> "Mult()"
        | Div -> "Div()"
        | Mod -> "Mod()") ^ ", " ^ string_of_expr e2 ^ ")"      
      |Eq|Neq|Lt|Leq|Gt|Geq -> "Compare(" ^ string_of_expr e1 ^ ", [" ^
        (match o with 
        | Eq -> "Eq()"
        | Neq -> "NotEq()"
        | Lt -> "Lt()"
        | Leq -> "LtE()"
        | Gt -> "Gt()"
        | Geq -> "GtE()") ^ "], [" ^ string_of_expr e2 ^ "])"
      |And|Or -> "BoolOp(" ^
        (match o with
	    | And -> "And()"
        | Or -> "Or()") ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ "]" )
  | Unop(o, e1) ->
    "UnaryOp(" ^
      (match o with
        Not -> "Not()"
        | Neg -> "USub()")  ^ string_of_expr e1 ^ ")"
  | Assign(v, e) -> (if List.mem v !attributesref then
      "Assign([Attribute(Name('self', Load()), '" ^ v ^ "', Store())], "  else
      "Assign([Name('" ^ v ^ "'), Store()], ") ^  string_of_expr e ^ ")"
  | Call(f, el) ->
    "Call(Attribute(Name('self', Load()), '" ^ f ^ "', Load()), [" ^ String.concat ", " (List.map string_of_expr el) ^ "], [], None, None)"
  | Noexpr -> ""

let string_of_compare compare = 
  "Compare(" ^ string_of_expr compare.left ^ ", [" ^ 
    (match compare.cmpop with 
        Eq -> "Eq()" | Neq -> "NotEq()" 
      | Lt -> "Lt()" | Leq -> "LtE()" 
      | Gt -> "Gt()" | Geq -> "GtE()" )
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
    VarDecl(x, y, z) -> string_of_expr (Assign(y,z))

let string_of_formal = function
    Formal(x, y) -> y

let string_of_fdecl fdecl =
  "FunctionDef('" ^ fdecl.fname ^ "', arguments([Name('self', Param())" ^ 
    (if fdecl.formals = [] then "]" else
      (List.fold_left (fun x y -> x ^ ", " ^ y) "" (List.map (fun x -> "Name('" ^ string_of_formal x ^ "', Param())") fdecl.formals) ^ "]")) ^ ", None, None, []), [" 
     ^ (if fdecl.locals = [] then "" else (String.concat ", " (List.map string_of_vdecl fdecl.locals))  ^ ", ") 
     ^ (String.concat ", " (List.map string_of_stmt fdecl.body)) ^ "], [])"

let string_of_node_vars = function
  | VarDecl(t, v, e) -> v

let string_of_compute n = 
  string_of_fdecl ({ return_type = VoidType; fname = "compute"; formals = n.args; locals = n.local_vars; body = n.compute })

let string_of_node ndecl = 
  attributesref := (List.map string_of_node_vars ndecl.local_vars);
  "ClassDef('" ^ ndecl.nname ^ "', [], " ^ "[" ^ string_of_compute ndecl ^ ", " ^ String.concat ", " (List.map string_of_fdecl ndecl.functions) ^ "], [])"

(* this is kinda bad, because it can only handle one node right now, and does no forwarding *)
let string_of_program nodes =
  "Module([Assign([Name('nodes', Store())], Dict([], [])), FunctionDef('forward', arguments([Name('node', Param()), Name('args', Param())], None, None, []), [Expr(Call(Attribute(Subscript(Name('nodes', Load()), Index(Name('node', Load())), Load()), 'compute', Load()), [], [], Name('args', Load()), None))], []), " ^ (String.concat ", " (List.map string_of_node nodes)) 
  ^ ", FunctionDef('main', arguments([], None, None, []), [Expr(Call(Attribute(Call(Name('" ^ (List.hd nodes).nname
  ^ "', Load()), [], [], None, None), 'compute', Load()), [], [], None, None))], []), Expr(Call(Name('main', Load()), [], [], None, None))" ^ "])"

let v = VarDecl(StringType, "x", StringLiteral("mesg"))
let f = { return_type = VoidType; fname = "sayhi"; formals = [Formal(StringType, "mesg")]; locals = []; body = [Print(Id("mesg"))] }
let n = [{ nname="hi"; args=[]; local_vars=[VarDecl(IntType, "x", IntLiteral(1))]; compute=[Expr(Call("sayhi", [StringLiteral("hi")]))]; functions=[f]}]

  *)















(*
a = """class hi():\n\n    def compute(self):\n        self.x = 1\n        print self.x"""

class hi():

    def compute(self):
        self.x = 1
        print self.x

"Module([ClassDef('hi', [], [FunctionDef('compute', arguments([Name('self', Param())], None, None, []), [Assign([Attribute(Name('self', Load()), 'x', Store())], Num(1)), Print(None, [Attribute(Name('self', Load()), 'x', Load())], True)], [])], [])])"
*)

(*
a = """class test():\n    def compute(self):\n        print self.fib(3)\n\n    def fib(self, x):\n        if (x < 2):\n            return 1\n        return self.fib(x-1)+self.fib(x-2)\n\ndef main():\n    test().compute()\n\nmain()"""

class test():
    def compute(self):
        print self.fib(3)

    def fib(self, x):
        if (x < 2):
            return 1
        return self.fib(x-1)+self.fib(x-2)

def main():
    test().compute()

main()

"Module([ClassDef('test', [], [FunctionDef('compute', arguments([Name('self', Param())], None, None, []), [Print(None, [Call(Attribute(Name('self', Load()), 'fib', Load()), [Num(3)], [], None, None)], True)], []), FunctionDef('fib', arguments([Name('self', Param()), Name('x', Param())], None, None, []), [If(Compare(Name('x', Load()), [Lt()], [Num(2)]), [Return(Num(1))], []), Return(BinOp(Call(Attribute(Name('self', Load()), 'fib', Load()), [BinOp(Name('x', Load()), Sub(), Num(1))], [], None, None), Add(), Call(Attribute(Name('self', Load()), 'fib', Load()), [BinOp(Name('x', Load()), Sub(), Num(2))], [], None, None)))], [])], []), FunctionDef('main', arguments([], None, None, []), [Expr(Call(Attribute(Call(Name('test', Load()), [], [], None, None), 'compute', Load()), [], [], None, None))], []), Expr(Call(Name('main', Load()), [], [], None, None))])"
*)

(*"
nodes = {}

def forward(node, args):
    nodes[node].compute(*args)

class node1():

    def compute(self):
        forward('node2', ["hi"])

class node2():

    def compute(self, arg1):
        print arg1

def main():
    global nodes
    nodes = { 'node1': node1(), 'node2': node2() }
    nodes['node1'].compute()

main()
"*)

(*
"Module([Assign([Name('nodes', Store())], Dict([], [])), FunctionDef('forward', arguments([Name('node', Param()), Name('args', Param())], None, None, []), [Expr(Call(Attribute(Subscript(Name('nodes', Load()), Index(Name('node', Load())), Load()), 'compute', Load()), [], [], Name('args', Load()), None))], []), ClassDef('node1', [], [FunctionDef('compute', arguments([Name('self', Param())], None, None, []), [Expr(Call(Name('forward', Load()), [Str('node2'), List([Str('hi')], Load())], [], None, None))], [])], []), ClassDef('node2', [], [FunctionDef('compute', arguments([Name('self', Param()), Name('arg1', Param())], None, None, []), [Print(None, [Name('arg1', Load())], True)], [])], []), FunctionDef('main', arguments([], None, None, []), [Global(['nodes']), Assign([Name('nodes', Store())], Dict([Str('node1'), Str('node2')], [Call(Name('node1', Load()), [], [], None, None), Call(Name('node2', Load()), [], [], None, None)])), Expr(Call(Attribute(Subscript(Name('nodes', Load()), Index(Str('node1')), Load()), 'compute', Load()), [], [], None, None))], []), Expr(Call(Name('main', Load()), [], [], None, None))])"y
*)
