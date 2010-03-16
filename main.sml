structure Main =
struct

fun run(program: Ast.prog as Ast.Prog(decls, exp))  =
  let
      val typ = TypeCheck.typeCheck program
                handle TypeCheck.TypeError t
                         => raise TypeCheck.TypeError ("type checker: " ^ t)
      val value = Eval.eval program
                  handle Eval.RunError t
                           => raise Eval.RunError ("evaluator: " ^ t)
  in
      TextIO.print(
      "decls:\n" ^ foldr(fn(a, b)=>"  " ^ PP.declToStr a^b) "" decls ^ "\n" ^
      "exp: "   ^ PP.expToStr exp ^ "\n" ^
      "   = " ^ PP.valToStr value ^ " : " ^ PP.tyToStr typ ^ "\n\n\n")
  end
end
