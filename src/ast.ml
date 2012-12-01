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
