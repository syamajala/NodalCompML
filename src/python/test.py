from ast import *
aststr = "Module([Assign([Name('nodes', Store())], Dict([], [])), FunctionDef('forward', arguments([Name('node', Param()), Name('args', Param())], None, None, []), [Expr(Call(Attribute(Subscript(Name('nodes', Load()), Index(Name('node', Load())), Load()), 'compute', Load()), [], [], Name('args', Load()), None))], []), ClassDef('add', [], [FunctionDef('compute', arguments([Name('self', Param())], None, None, []), [Print(None, [BinOp(Num(39), Add(), Num(3))], True)], []), ], []), FunctionDef('main', arguments([], None, None, []), [Expr(Call(Attribute(Call(Name('add', Load()), [], [], None, None), 'compute', Load()), [], [], None, None))], []), Expr(Call(Name('main', Load()), [], [], None, None))])"
ast = eval(aststr)
fix_missing_locations(ast)
obj = compile(ast, "", 'exec')
exec obj