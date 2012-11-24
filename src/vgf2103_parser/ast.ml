type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater

type expr = 
    Literal of int
  | Id of string
  | Binop of expr * op * expr
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
