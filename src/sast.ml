open Ast

type simple_expr = 
	| CharLit of char
	| StringLit of string
	| IntLit of int
	| FloatLit of float
	| BoolLit of bool
	| Id of string
	| Binop of expr * binop * expr
	| Unop of unop * expr
	| Assign of string * expr
	| Call of string * expr list
	| Noexpr

and expr = simple_expr * dtype

and stmt = 
	| Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
	| Print of expr
	| Nostmt 

(*and var_decl = {
	vname : string;
	vtype : dtype;
}*)

and var_decl = string * dtype

and func_decl = {
	ret_type : dtype;
	func_name : string;
	fparams : (string * dtype) list;
	flocals : (string * dtype) list;
	fbody : stmt list;
	unchecked_fbody : (Ast.stmt) list;
}

and node_decl = {
	node_name : string;
	nparams : (string * dtype) list;
	nlocals : (string * dtype) list;
	unchecked_body : (Ast.stmt) list;
	nbody : stmt list;
	helper_funcs : func_decl list;
}

and program = node_decl list

