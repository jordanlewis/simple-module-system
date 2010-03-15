structure Main =
struct

fun run(program: Ast.prog) =
  let val () = TextIO.print("started type check\n")
      val typ = TypeCheck.typeCheck program
      val () = TextIO.print("finished type check\n")
      val value = Eval.eval program
  in
      TextIO.print("type:  " ^ Ast.tyToStr typ ^ "\n" ^
                   "value: " ^ Eval.valToStr value ^ "\n")
  end
end
