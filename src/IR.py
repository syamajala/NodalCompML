from ast import *
aststr="Module([ClassDef('blur', [], [FunctionDef('compute', arguments([Name('self', Param())], None, None, []), [Print(None, [Str('"666"')], True)], []), ], []), FunctionDef('main', arguments([], None, None, []), [Expr(Call(Attribute(Call(Name('blur', Load()), [], [], None, None), 'compute', Load()), [], [], None, None))], []), Expr(Call(Name('main', Load()), [], [], None, None))])"
ast=eval(aststr)
ast=fix_missing_locations(ast)
obj=compile(ast,"","exec")
exec obj