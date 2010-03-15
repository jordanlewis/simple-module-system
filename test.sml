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
               TYDECL("intcopy", NONE, INT),
               VALDECL("z", NUM 7),
               MODDECL("submod", MOD([VALDECL("p", NUM 6)])),
               MODDECL("submodcopy", MVAR("submod"))])),
        VALDECL("n", NUM 5),
        MODDECL("mymodcopy", MVAR("mymod")),
        TYDECL("myint", NONE, INT)],
       APPLY(TYAPPLY(PATH(["mymod"], "id"), INT),
             PATH(["mymodcopy", "submodcopy"], "p")))
val tyOfDeclTest = Main.run decltest

end
