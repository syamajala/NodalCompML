open Ast
open Sast

type symbol_table = {
	parent : symbol_table option;
	vars : Sast.var_decl list;
	funcs : Sast.func_decl list;
}

type env = {
	scope : symbol_table;
	nodes : node_decl list;
}

let print_var (var:Sast.var_decl) =
	let (name, _) = var in print_string name

let print_node node = 
	let name = node.node_name in print_string name

let find_node_var env (node_name : string) var_name =
	let node = List.find (fun n -> n.node_name = node_name) env.nodes
	in let locals = node.nlocals
	in try
		print_string ("\nCheck locals of node " ^ node_name ^ " ");
		List.iter print_var locals;
		List.find (fun (name, _) -> name = var_name) locals
	with Not_found ->
		raise (Failure("variable " ^ var_name ^ " not defined"))

let rec find_var env scope node name = 
	try
		print_string("\nCheck vars in scope: ");
		List.iter print_var scope.vars;
		List.find (fun (var_name, _) -> var_name = name) scope.vars
	with Not_found ->
		match scope.parent with
			| Some(parent) -> find_var env parent node name
			| _ -> find_node_var env node.node_name name

let rec find_func (scope : symbol_table) name = 
	try 
		List.find (fun f -> f.fname = name) scope.funcs
	with Not_found ->
		match scope.parent with
			| Some(parent) -> find_func parent name
			| _ -> raise (Failure("function " ^ name ^ " not defined"))

let rec find_node env name = 
	try 
		List.find (fun node -> node.node_name = name) env.nodes
	with Not_found -> 
		raise (Failure("node " ^ name ^ " not defined"))

let same_type t arg = 
	let (_, arg_type) = arg in
		if t = arg_type then arg
		else raise (Failure("mismatched types"))

let rec expr env node = function
	| Ast.Noexpr -> Sast.Noexpr, VoidType
	| Ast.CharLiteral(lit) -> Sast.CharLit(lit), CharType
	| Ast.StringLiteral(lit) -> Sast.StringLit(lit), StringType
	| Ast.IntLiteral(lit) -> Sast.IntLit(lit), IntType
	| Ast.FloatLiteral(lit) -> Sast.FloatLit(lit), FloatType
	| Ast.BoolLiteral(lit) -> Sast.BoolLit(lit), BoolType
	| Ast.Id(id) ->
		print_string("\nFound id " ^ id ^", verify in scope ");
		let vdecl = try
			find_var env env.scope node id
		with Not_found ->
				raise (Failure ("undeclared identifier: " ^ id))
		in
			let (name, typ) = vdecl in
				Sast.Id(name), typ
	| Ast.Binop (e1, op, e2) ->
		let e1 = expr env node e1
		and e2 = expr env node e2 in
			let (_, t1) = e1
			and (_, t2) = e2 in
				let t = 
					if (not (t1 = t2)) then
						raise (Failure ("type mismatch"))
					else match op with 
						| Ast.Add -> (match t1 with
							| IntType | FloatType | StringType -> t1
							| _ -> raise (Failure ("Type failure")))
						| Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod -> (match t1 with
							| IntType | FloatType -> t1
							| _ -> raise (Failure ("Type failure")))
						| Ast.Eq | Ast.Neq -> (match t1 with
							| IntType | FloatType | CharType 
							| StringType | BoolType -> BoolType
							| _ -> raise (Failure ("Type failure")))
						| Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq -> (match t1 with
							| IntType | FloatType | CharType | StringType -> BoolType
							| _ -> raise (Failure ("Type failure")))
						| Ast.And | Ast.Or -> (match t1 with
							| BoolType -> BoolType
							| _ -> raise (Failure ("Mismatched types")))
				in Sast.Binop(e1, op, e2), t
	| Ast.Unop (op, e) ->
		let e = expr env node e in
			let (_, t) = e in
				let t = match op with
					| Ast.Neg -> (match t with
						| IntType | FloatType -> t
						| _ -> raise (Failure ("Type failure")))
					| Ast.Not -> (match t with
						| BoolType -> BoolType
						| _ -> raise (Failure ("Type failure")))
				in Sast.Unop(op, e), t
	| Ast.Assign (id, e) ->
			let id = find_var env env.scope node id
			and e = expr env node e in
				let (name, t1) = id 
				and (_, t2) = e in
					if (t1 = t2) then Sast.Assign(name, e), t1
					else raise (Failure ("Type mismatch in assignment"))
	| Ast.Call (func_name, params) ->
		let func =
			try 
				List.find (fun f -> f.fname = func_name) env.scope.funcs
			with Not_found ->
				raise (Failure ("undeclared identifier " ^ func_name))
		in
			let typed_params = List.map (expr env node) params in
			let types = List.map (fun e_type -> snd e_type) typed_params in
				try
					Sast.Call (func.fname, List.map2 same_type types typed_params), func.return_type
				with Invalid_argument(x) -> raise (Failure("Invalid number of args"))

let rec stmt env node = function
	| Ast.Block(s1) ->
			let s1 = List.map (fun s -> stmt env node s) s1
				in Sast.Block(s1)
	| Ast.Expr(e) -> Sast.Expr(expr env node e)
	| Ast.Return(e) -> Sast.Return(expr env node e)
	| Ast.If(e, s1, s2) ->
		let e = expr env node e in
			if ((snd e) = BoolType) then
				Sast.If(e, stmt env node s1, stmt env node s2)
			else 
				raise (Failure ("If condition must evaluate to a boolean"))
	| Ast.For (e1, e2, e3, s) ->
		let e2 = expr env node e2 in
			if ((snd e2) = BoolType) then
				Sast.For (expr env node e1, e2, expr env node e3, stmt env node s)
			else
				raise (Failure ("Continuation condition in For loop must be boolean"))
	| Ast.While (e, s) ->
		let e = expr env node e in
			if ((snd e) = BoolType) then
				Sast.While (e, stmt env node s)
			else
				raise (Failure ("While condition must be boolean"))
	| Ast.Print (e) -> 
		let e = expr env node e in Sast.Print(e)
	| Ast.Nostmt -> Sast.Nostmt
	| _ -> raise (Failure ("Error"))


(*let check_unop t = function 
	| Ast.Neg -> (match t with
			| Int | Float -> t
			| _ -> raise (Failure ("Type failure")))
	| Ast.Not -> (match t with
			| Boolean -> Boolean
			| _ -> raise (Failure ("Type failure")))
	| Ast.Inc -> (match t with
			| Int | Float -> t
			| _ -> raise (Failure ("Type failure")))
	| _ -> raise (Failure ("Not a valid unop"))

let check_binop t1 t2 = 
	if (not (t1 = t2)) then
		raise (Failure ("type mismatch"))
	else match binop with 
		| Ast.Add -> (match t1 with
				| Int | Float | String -> t1
				| _ -> raise (Failure ("Type failure")))
		| Ast.Sub | Ast.Mult | Ast.Div | Ast.Mod -> (match t1 with
				| Int | Float -> t1
				| _ -> raise (Failure ("Type failure")))
		| Ast.Eq | Ast.Neq -> (match t1 with
				| Int | Float | Char | String | Boolean -> Boolean
				| _ -> raise (Failure ("Type failure")))
		| Ast.Lt | Ast.Gt | Ast.Leq | Ast.Geq -> (match t1 with
				| Int | Float | Char | String -> Boolean
				| _ -> raise (Failure ("Type failure")))
		| Ast.And | Ast.Or -> (match t1 with
				| Boolean -> Boolean
				| _ -> raise (Failure ("Mismatched types")))

let same_type t arg = 
	let (_, arg_type) = arg in
		if t = arg_type then arg
		else raise (Failure("mismatched types"))

let check_func types args = 
	try
		List.map2 same_type types args
	with Invalid_arguemnt(x) ->
		raise (Failure("Invalid number of arguments"))

let check program =
	let rec check_expr env = function
		(* put in constants *)
		| Ast.Id(id) -> Sast.Id(find_var scope name), var.vtype
		| Ast.Binop(e1, op, e2) ->
			let e1_type = check_expr scope e1
			and e2_type = check_expr scope e2 in
				let (_, t1) = e1_type 
				and (_, t2) = e2_type in
					Sast.Binop(e1_type, op, e2_type), check_binop t1 t2
		| Ast.Unop(unop, e) -> 
			let e_type = check_expr scope e in
				let (_, t) = e_type in 
					Sast.Unop(op, e_type), check_unop t
		| Ast.Assign(id, e) ->
			let id_type = find_var scope id
			and e_type = check_expr scope e in
				let (t1, _) = id_type
				and (_, t2) = e_type in
					if (t1 = t2) then Sast.Assign(snd id_type, e_type), t1
					else raise (Failure("Type mismatch in assignment"))
		| Ast.Call(func_name, params) ->
			let args = List.map (fun s -> check_expr env s) params in
				let fdecl = find_function env.scope func_name in
					let types = List.rev (List.map (fun v -> v.vtype) (List.rev fdecl.params))
						in try
							Sast.Call(fdecl, List.mape2 same_type types args), fdecl.return_type
							with Invalid_arguemnt(x) ->
								raise (Failure("Invalid number of arguments"))
	 in let add_local env var = 
		if (var_exists env.scope var.vname) then 
			raise (Failure("variable " ^ var.vname ^ " already in scope"))
		else
			let new_var = {
				vname = v.vname;
				vtype = v.vtype;
			} in
				let vars_in_scope = new_v :: env.scope.vars in
					let scope = {
						env.scope with vars = vars_in_scope
					}
	in let rec check_stmt env = function
		| Ast.Block(vars, stmts) ->
			let block_scope = {
				parent = Some(env.scope);
				vars = [];
				funcs = [];
			} in 
				let new_env = {
					scope = block_scope;
				} in
					let block_env = List.fold_left add_local new_env (List.rev vars) in
						let block_stmts = List.map (fun s -> check_stmt block_env s) stmts in
							Sast.Block(block_env.scope.vars, block_stmts)
		| Ast.Expr(e) ->
			Sast.Expr(check_expr env e)
		| Ast.Return(e) ->
			Sast.Return(check_expr env e)
		| Ast.If(e, s1, s2) ->
			let e_type = check_expr env e in
				if ((snd e_type) = Boolean) then
					Sast.If(e, check_stmt env s1, check_stmt env s2)
				else
					raise (Failure("If condition must be of type boolean"))
		| Ast.While(e, s) ->
			let e_type = check_expr env e in
				if ((snd e_type) = Boolean) then
					Sast.While(e_type, check_stmt env s)
	in let add_func env func = 
		if ((func_exists env.scope func.fname) || var_exists env.scope func.fname) then
			raise (Failure("Identifier with name " ^ func.fname ^ "  already in scope"))
		else
			let func_scope = {
				parent = Some(env.scope);
				variables = [];
				funcs = [];
			} in
				let func_env = {
					scope = func_scope;
				} in let func_env = List.fold_left add_local func_env (List.rev func.params) 
					in {
						return_type = func.ftype;
						fname = func.fname;
						params = func_env.scope.vars;
						locals = [];
						body = [];
					} in let funcs = new_f :: env.scope.funcs
						in let func_scope = {
							env.scope with funcs = funcs}
							in {scope = func_scope}
(* 	in let check_func env (f : Ast.func_decl) = 
			Do this and validate_func *)
	in let global_scope = {
		parent = None;
		variables = [];
		functions = [];
	}
	in let global_env = {
		scope = global_scope;
	}
	(* add nodes to global env *)
	in check_expr program.expr	*)
	
let node_exists nodes name = 
	List.exists (fun n -> n.node_name = name) nodes

let add_param env node param = 
	let Ast.Formal(typ, name) = param
	in let params = (name, typ) :: node.nparams
	and scope_vars = (name, typ) :: env.scope.vars
	in let scope = {
		env.scope with vars = scope_vars
	} in 
	let _ = {
		env with scope = scope
	}
	in {
		node with nparams = params
	}

let add_local env node var = 
	let Ast.VarDecl(typ, name, e) = var in
	print_string("\nadding local: " ^ name);
	let scope_vars = node.nparams @ node.nlocals
	in let scope = {
		env.scope with vars = scope_vars
	} in
	let env = {
		env with scope = scope
	}
	in let (_, t) = expr env node e
	in if (t != typ) then 
		raise (Failure ("type error when initializing id " ^ name))
	else
		let var_list = (name, typ) :: node.nlocals
		in  {
			node with nlocals = var_list
		}

let add_compute (env:env) node s =
	let stmts = s :: node.unchecked_body
	in {
		node with unchecked_body = stmts
	}

let add_node env (node : Ast.node) =
	if (node_exists env.nodes node.nname) then 
		raise (Failure ("Node with name " ^ node.nname ^ " already exists"))
	else
		let new_node = {
			node_name = node.nname;
			nparams = [];
			nlocals = [];
			nbody = [];
			unchecked_body = [];
			helper_funcs = [];
		} in let new_node = List.fold_left (add_param env) new_node (List.rev node.args) 
		in let new_node = List.fold_left (add_local env) new_node (node.local_vars) 
		in 
		print_string("\nAdded locals to node: ");
		List.iter print_var new_node.nlocals;
		let new_node = List.fold_left (add_compute env) new_node (List.rev node.compute)	
(*		in let new_node = List.fold_left add_func new_node (List.rev node.functions) *)
		in let new_nodes = (new_node :: env.nodes)
		in {
			env with nodes = new_nodes
		}

let check_compute (env:env) node s =
	let s = stmt env node s in
	let stmts = s :: node.nbody
	in {
		node with nbody = stmts
	}

let check_node env (node:Sast.node_decl) =
	print_string("\nChecking compute function"); 
	let new_node = {
		node_name = node.node_name;
		nparams = node.nparams;
		nlocals = node.nlocals;
		nbody = [];
		unchecked_body = node.unchecked_body;
		helper_funcs = node.helper_funcs;
	}
	in let node = List.fold_left (check_compute env) new_node node.unchecked_body
	in let node_list = node :: env.nodes
	in {
		env with nodes = node_list
	}

let check_start_node node_list =
	try
		let start_node = List.find (fun n -> n.node_name = "start") node_list
			in true
			(* Do we want to check params here? *)
	with Not_found ->
		false

let check program =
	print_string("\nStarting semantic analysis\n"); 
	let global_scope = {
		parent = None;
		vars = [];
		funcs = [];
	} in 
		let global_env = {
			scope = global_scope;
			nodes = [];
		} in let global_env = List.fold_left add_node global_env program
		in
		print_string("\nFinished first pass through nodes");
		print_string("\nNode List: ");
		List.iter print_node global_env.nodes;
		if (check_start_node global_env.nodes) then
			List.fold_left check_node global_env global_env.nodes
		else
			raise (Failure ("No start node found"))
