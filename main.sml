structure Main =
struct

fun run(program: Ast.prog as Ast.Prog(decls, exp))  =
  let
      val typ = TypeCheck.typeCheck program
               handle TypeCheck.TypeError s
                             => raise TypeCheck.TypeError ("type checker: " ^ s)
                    | Fail s => raise Fail ("type checker: " ^ s)
      val value = Eval.eval program
               handle Eval.RunError s => raise Eval.RunError ("evaluator: " ^ s)
                    | Fail s => raise Fail ("evaluator: " ^ s)
  in
      TextIO.print(
      "decls:\n" ^ foldr(fn(a, b)=>"  " ^ PP.declToStr a^b) "" decls ^ "\n" ^
      "exp: "   ^ PP.expToStr exp ^ "\n" ^
      "   = " ^ PP.valToStr value ^ " : " ^ PP.tyToStr typ ^ "\n\n\n")
  end
end
