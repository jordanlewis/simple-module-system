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
          MOD([VALDECL("id", TYFN("a", FN("x", TYVARARG "a", VAR "x"))),
               TYDECL("intcopy", NONE, INT),
               VALDECL("z", NUM 7),
               MODDECL("submod", MOD([VALDECL("p", NUM 6)])),
               MODDECL("submodcopy", MVAR("submod"))])),
        VALDECL("n", NUM 5),
        MODDECL("mymodcopy", MVAR("mymod")),
        TYDECL("myint", NONE, INT)],
       APPLY(TYAPPLY(PATH(["mymod"], "id"), TYPATH(["mymodcopy"], "intcopy")),
             PATH(["mymodcopy", "submodcopy"], "p")))
val () = Main.run decltest

val prog2 =
  Prog([TYDECL("myint", NONE, INT),
  MODDECL("mymod",
          MOD([VALDECL("foo", FN("x", TYVAR "myint", VAR "x"))]))],
       APPLY(PATH(["mymod"], "foo"), (NUM 6)))

val () = Main.run prog2

val prog3 =
  Prog([TYDECL("myint", NONE, INT),
        VALDECL("mynum", NUM 8),
        MODDECL("mymod",
          MOD([TYDECL("innermyint", NONE, TYVAR "myint"),
               VALDECL("innermynum", VAR "mynum"),
               VALDECL("foo", FN("x", TYVAR "innermyint",
                                  PRIM(PLUS, VAR "x", VAR "innermynum")))])),
        VALDECL("test", FN("x", TYPATH(["mymod"], "innermyint"),
                        APPLY(PATH(["mymod"], "foo"), VAR "x")))],
       APPLY(VAR "test", NUM 6))

val () = Main.run prog3


end
