(* syntax.sml *)

(** Abstract syntax datatypes **)

structure Ast =
struct

datatype operator = PLUS | TIMES | MINUS | EQUAL | LESS

type varname = string
type tyname = string
type modname = string

datatype ty = INT
            | BOOL
            | FUNCTION of ty * ty
            | TYVAR of tyname
            | TYPATH of modname list * tyname
            | POLY of tyname * ty

datatype exp = VAR of varname
             | PATH of modname list * varname
             | NUM of int
             | PRIM of (operator * exp * exp) (* Binary ops only *)
             | TRUE
             | FALSE
             | IF of (exp * exp * exp) (* if exp1 then exp2 else exp3*)
             | FN of varname * ty * exp
             | TYFN of tyname * exp
             | APPLY of exp * exp
             | TYAPPLY of exp * ty
             | LET of (varname * ty * exp * exp) (* x:ty=exp1 in exp2 *)

datatype decl = TYDECL of tyname * string option * ty (* type foo ['a] = ty *)
              | VALDECL of varname * exp   (* val foo = exp *)
              | MODDECL of modname * modexp

and modexp = MOD of decl list
           | MVAR of modname


datatype prog = Prog of decl list * exp


fun tyToStr (typ: ty) =
  (case typ
     of INT => "INT"
      | BOOL => "BOOL"
      | FUNCTION (t1, t2) => tyToStr t1 ^ " -> " ^ tyToStr t2
      | TYVAR t => "tyvar(" ^ t ^ ")"
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
                                  " = " ^ tyToStr t ^ "\n")

fun modexpToStr (modexpr: modexp) =
  (case modexpr
     of MOD (decls) => "mod " ^ (foldr(fn(a:decl, b)=>declToStr a^b) "" decls) ^
                       "end\n"
      | MVAR (name) => "mod " ^ name)

fun progToStr (program as Prog(decls, e)) =
  (case decls
     of [] => ""
      | _ => (foldr (fn(a:decl, b) => declToStr a ^ b) "" decls) ^ "\n\n" ^
              expToStr e ^ "\n")
end
