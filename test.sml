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

val () =
  let val testCases = [

(* Test module references and paths *)
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

,

Prog([TYDECL("myint", NONE, INT),
      MODDECL("mymod",
        MOD([VALDECL("foo", FN("x", TYVAR "myint", VAR "x"))]))],
     APPLY(PATH(["mymod"], "foo"), (NUM 6)))

,

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

,

Prog([MODDECL("mymod",
        MOD([MODDECL("innermod",
                     MOD([TYDECL("myint", NONE, INT)])),
             VALDECL("foo", FN("x", TYPATH(["innermod"], "myint"), VAR "x"))
             ]))],
      APPLY(PATH (["mymod"], "foo"), NUM 6))

,

(* Make sure baseTy (FUNCTION (x, y)) = FUNCTION (baseTy (x), baseTy(y)) *)
Prog([MODDECL("m1",
        MOD [TYDECL("t1", NONE, INT),
             MODDECL("m2",
               MOD([VALDECL("f", FN("x", FUNCTION(TYVAR "t1", INT),
                                           APPLY(VAR "x", NUM 2)))]))]),
      VALDECL("g", FN("y", INT, VAR "y"))],
     APPLY(PATH(["m1","m2"], "f"), VAR "g"))
,


(* Same as above for baseTy (POLY _) *)
Prog([MODDECL("m1",
        MOD [TYDECL("t1", NONE, INT),
             MODDECL("m2",
               MOD([VALDECL("f", TYFN("a", FN("x", FUNCTION(TYVAR "t1",
               TYVARARG "a"), APPLY(VAR "x", NUM 2))))]))]),
      VALDECL("g", FN("y", INT, VAR "y"))],
     APPLY(TYAPPLY(PATH(["m1","m2"], "f"), INT), VAR "g"))
,


(* Make sure that ALL(q) q->int = ALL(a) a->int *)
Prog([VALDECL("f", FN("x", POLY("q", FUNCTION(TYVARARG"q", BOOL)),
                      APPLY(TYAPPLY(VAR "x", INT), NUM 3)))],
     APPLY(VAR "f", TYFN("a", FN("b", TYVARARG "a", TRUE))))
]

in
  app Main.run testCases
end

end
