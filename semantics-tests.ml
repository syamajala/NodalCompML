(* need to add Ast.Nostmt to let rec stmt *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Lt, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(3)), Ast.Nostmt)]; functions = [] };;
check[n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Lt, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(2)), Ast.Expr(IntLiteral(3)))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(StringType, "s", StringLiteral("hi"))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Lt, Ast.IntLiteral(2)), Ast.Nostmt, Ast.Nostmt)]; functions = [] };;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Gt, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(2)), Ast.Expr(IntLiteral(3)))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(2)), Ast.Expr(IntLiteral(3)))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(2)), Ast.Expr(IntLiteral(3)))]; functions = [] };;
let n1 = { nname = "test1"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(2)), Ast.Expr(IntLiteral(3)))]; functions = [] };;
check [n; n1];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(2)), Ast.Expr(IntLiteral(3)))]; functions = [] };;
let n1 = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.If(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(IntLiteral(2)), Ast.Expr(IntLiteral(3)))]; functions = [] };;
check [n; n1];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.While(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(Ast.Assign("x", IntLiteral(1))))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.While(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(Ast.Assign("s", IntLiteral(2))))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.While(Ast.Binop(Ast.Id("s"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(Ast.Assign("s", StringLiteral("H"))))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.While(Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Expr(Ast.Assign("s", IntLiteral(1))))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.While(Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Expr(Ast.Assign("s", IntLiteral(1))))]; functions = [] };;

(* Nostmt failure again *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.For(Ast.Noexpr, Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Assign("s", IntLiteral(2)), Ast.Nostmt)]; functions = [] };;
check[n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.For(Ast.Noexpr, Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Assign("s", IntLiteral(2)), Ast.Print(Ast.Id("s")))]; functions = [] };;
check[n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.For(Ast.Assign("p", Ast.StringLiteral("hi")), Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Assign("s", IntLiteral(2)), Ast.Print(Ast.Id("s")))]; functions = [] };;
check[n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.For(Ast.Assign("s", Ast.StringLiteral("hi")), Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Assign("s", IntLiteral(2)), Ast.Print(Ast.Id("s")))]; functions = [] };;
check[n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(IntType, "s", IntLiteral(1))]; compute = [Ast.For(Ast.Assign("s", Ast.IntLiteral(4)), Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Assign("s", Ast.IntLiteral(2)), Ast.Print(Ast.Id("s")))]; functions = [] };;
let n1 = { nname = "test2"; args = []; local_vars = [VarDecl(IntType, "s2", IntLiteral(1))]; compute = [Ast.While(Ast.Binop(Ast.Id("s2"), Ast.Eq, Ast.IntLiteral(2)), Ast.Expr(Ast.Assign("s2", Ast.IntLiteral(3))))]; functions = [] };;
check [n; n1];;

(* maybe good? *)
let n = { nname = "test"; args = [Formal(CharType, "s")]; local_vars = []; compute = [Ast.For(Ast.Assign("s", Ast.IntLiteral(4)), Ast.Binop(Ast.Id("s"), Ast.Neq, Ast.IntLiteral(2)), Ast.Assign("s", Ast.IntLiteral(2)), Ast.Print(Ast.Id("s")))]; functions = [] };;
check [n];;

(* fails *)
let n = { nname = "test"; args = [Formal(CharType, "s")]; local_vars = []; compute = [Ast.Print(Ast.Id("s"))]; functions = [] };;
check [n];;

(* fails *)
let n = { nname = "test"; args = [Formal(CharType, "s")]; local_vars = [VarDecl(CharType, "t", Ast.CharLiteral('a'))]; compute = [Ast.Expr(Ast.Assign("t", Ast.Id("s")))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(CharType, "t", Ast.CharLiteral('a'))]; compute = [Ast.Return(IntLiteral(1))]; functions = [] };;
check [n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(CharType, "t", Ast.CharLiteral('a'))]; compute = [Ast.Return(Ast.Id("t"))]; functions = [] };;
check[n];;

(* good *)
let n = { nname = "test"; args = []; local_vars = [VarDecl(CharType, "t", Ast.CharLiteral('a'))]; compute = [Ast.Return(Ast.Id("s"))]; functions = [] };;
check[n];;
