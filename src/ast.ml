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

type fun_decl = {
  fname : string;
  formals : string list;
  locals : string list;
  body : stmt list;
}

type program = string list * fun_decl list
