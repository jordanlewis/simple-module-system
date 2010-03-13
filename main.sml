structure Main =
struct

fun run(program: Ast.prog) =
  TextIO.print("type:  " ^ Ast.tyToStr (TypeCheck.typeCheck program) ^ "\n" ^
               "value: " ^ Eval.valToStr (Eval.eval program) ^ "\n")
end
