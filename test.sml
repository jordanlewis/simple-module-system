local
  open Syntax
in

val polyid  = TYFN("a", FN("x", TYVAR "a", TYVAR "a", VAR "x"))
val tyOfPolyid = TypeCheck.typeOf(Env.empty, polyid)
val valOfPolyid = Eval.eval(Env.newenv, polyid)
val polyidInt = TYAPPLY(polyid, INT)
val tyOfPolyidInt = TypeCheck.typeOf(Env.empty, polyidInt)
val valOfPolyidInt = Eval.eval(Env.newenv, polyidInt)


end
