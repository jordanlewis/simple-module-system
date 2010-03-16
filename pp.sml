local
  open Ast
  open Eval
in

structure PP =
struct

fun tyToStr (typ: ty) =
  (case typ
     of INT => "INT"
      | BOOL => "BOOL"
      | FUNCTION (t1, t2) => tyToStr t1 ^ " -> " ^ tyToStr t2
      | TYVAR t => "tyvar(" ^ t ^ ")"
      | TYVARARG t => "tyvararg(" ^ t ^ ")"
      | TYPATH (modlist, name) =>
          implode(tl(explode((foldr (fn (m, string) => string ^ "." ^ m) ""
          modlist) ^ "." ^ name)))
      | POLY (name, t) => "ALL(" ^ name ^ ")" ^ tyToStr t)

fun expToStr (expr: exp) =
  let fun optostr (opr: operator) =
            (case opr of PLUS => "+"
                       | TIMES => "*"
                       | MINUS => "-"
                       | EQUAL => "=="
                       | LESS => "<")
  in
    (case expr
       of VAR x => x
        | PATH (modlist, name) =>
          implode(tl(explode(foldr (fn (m, str) => str ^ "." ^ m) "" modlist ^
          "." ^ name)))
        | NUM n => Int.toString(n)
        | PRIM (opr, e1, e2) => expToStr e1 ^ " " ^ optostr(opr) ^ " " ^
                                expToStr e2
        | TRUE => "TRUE"
        | FALSE => "FALSE"
        | IF (pred, tt, ff) => "if " ^ expToStr pred ^ " then " ^
                              expToStr tt ^ " else " ^ expToStr ff
        | FN (var, t, e) => "fn(" ^ var ^ ":" ^ tyToStr t ^ ") = (" ^
                            expToStr e ^ ")"
        | TYFN (name, e) => "tyfn(" ^ name ^ ") = (" ^ expToStr e ^ ")"
        | APPLY (e1, e2) => expToStr e1 ^ "(" ^ expToStr e2 ^ ")"
        | TYAPPLY (e, t) => expToStr e ^ "[" ^ tyToStr t ^ "]"
        | LET (var, t, e1, body) => "let " ^ var ^ ":" ^ tyToStr t ^
                                    " be " ^ expToStr e1 ^ " in " ^
                                    expToStr body)
  end

fun declToStr (dec: decl) =
  (case dec
     of VALDECL (name, e) => "val " ^ name ^ " = " ^ expToStr e ^ "\n"
      | TYDECL (name, arg, t) => "type " ^ name ^
                                  (case arg
                                      of NONE => ""
                                      | SOME s => "[" ^ name ^ "]") ^
                                  " = " ^ tyToStr t ^ "\n"
      | MODDECL (name, modexpr)=>"module " ^ name ^ " =\n"^modexpToStr modexpr)

and modexpToStr (modexpr: modexp) =
  (case modexpr
     of MOD (decls) => "mod\n"^ (foldr(fn(a:decl, b)=>"  "^declToStr a^b) "" decls) ^
                       "end\n"
      | MVAR (name) => "mvar " ^ name ^ "\n")

fun progToStr (program as Prog(decls, e)) =
  (case decls
     of [] => ""
      | _ => (foldr (fn(a:decl, b) => declToStr a ^ b) "" decls) ^ "\n\n" ^
              expToStr e ^ "\n")

fun valToStr(v: value) =
  (case v
     of Num n => Int.toString n
      | Bool b => Bool.toString b
      | Closure (expr, env) => "Closure(" ^ expToStr expr ^ ", )" )


end
end
