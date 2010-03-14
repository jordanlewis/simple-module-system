local
  open Ast
in

(* Some very basic tests of polymorphism in the static and dynamic semantics.*)

(*
val polyid  = TYFN("a", FN("x", TYVAR "a", VAR "x"))
val tyOfPolyid = TypeCheck.typeOf(Env.empty, polyid)
val valOfPolyid = Eval.valOf(Env.empty, polyid)

val polyidInt = TYAPPLY(polyid, INT)
val tyOfPolyidInt = TypeCheck.typeOf(Env.empty, polyidInt)
val valOfPolyidInt = Eval.valOf(Env.empty, polyidInt)

val polyidIntFive = APPLY(TYAPPLY(polyid, INT), NUM 5)
val tyOfPolyidIntFive = TypeCheck.typeOf(Env.empty, polyidIntFive)
val valOfPolyidIntFive = Eval.valOf(Env.empty, polyidIntFive)
*)


val decltest =
  Prog([MODDECL("mymod",
          MOD([VALDECL("id", TYFN("a", FN("x", TYVAR "a", VAR "x"))),
               MODDECL("submod", MOD([VALDECL("p", NUM 6)]))])),
        VALDECL("n", NUM 5)],
       APPLY(TYAPPLY(PATH(["mymod"], "id"), INT),
                     PATH(["mymod", "submod"], "p")))
val tyOfDeclTest = Main.run decltest

end
