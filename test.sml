local
  open Syntax
in

(* Some very basic tests of polymorphism in the static and dynamic semantics.*)

val polyid  = FN(TyParam("a"), FUNCTION(TYVAR "a", TYVAR "a"), FN(VarParam("x", TYVAR "a"), TYVAR "a", VAR "x"))
val tyOfPolyid = TypeCheck.typeOf(Env.empty, polyid)
val valOfPolyid = Eval.eval(Env.newenv, polyid)

val polyidInt = APPLY(polyid, TyArg INT)
val tyOfPolyidInt = TypeCheck.typeOf(Env.empty, polyidInt)
val valOfPolyidInt = Eval.eval(Env.newenv, polyidInt)

val polyidIntFive = APPLY(polyidInt, ExpArg (NUM 5))
val tyOfPolyidInt = TypeCheck.typeOf(Env.empty, polyidIntFive)
val valOfPolyidInt = Eval.eval(Env.newenv, polyidIntFive)



end
