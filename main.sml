structure Main =
struct

fun run(program: Ast.prog) =
  let
      (*val () = TextIO.print("started type check\n")*)
      val typ = TypeCheck.typeCheck program
      (*val () = TextIO.print("finished type check, beginnning evaluation\n")*)
      val value = Eval.eval program
      (*val () = TextIO.print("finished evaluation\n")*)
  in
      TextIO.print("type:  " ^ Ast.tyToStr typ ^ "\n" ^
                   "value: " ^ Eval.valToStr value ^ "\n")
  end
end
