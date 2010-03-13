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

datatype decl = TYDECL of string * string option * ty (* type foo ['a] = ty *)
              | VALDECL of string * exp   (* val foo = exp *)
              | MODDECL of string * modexp

and modexp = MOD of decl list
           | MVAR of modname


datatype prog = Prog of decl list * exp


fun tostr (program as Prog(decls, e)) = 
  let fun tytostr (typ: ty) =
            (case typ
               of INT => "INT"
                | BOOL => "BOOL"
                | FUNCTION (t1, t2) => tytostr t1 ^ " -> " ^ tytostr t2
                | TYVAR t => "tyvar(" ^ t ^ ")"
                | POLY (name, t) => "ALL(" ^ name ^ ")" ^ tytostr t)
      fun optostr (opr: operator) =
            (case opr of PLUS => "+"
                      | TIMES => "*"
                      | MINUS => "-"
                      | EQUAL => "=="
                      | LESS => "<")
      fun exptostr (expr: exp) =
            (case expr
              of VAR x => x
               | NUM n => Int.toString(n)
               | PRIM (opr, e1, e2) => exptostr e1 ^ " " ^ optostr(opr) ^ " " ^ exptostr e2
               | TRUE => "TRUE"
               | FALSE => "FALSE"
               | IF (pred, tt, ff) => "if " ^ exptostr pred ^ " then " ^
                                      exptostr tt ^ " else " ^ exptostr ff
               | FN (var, t, e) => "fn(" ^ var ^ ":" ^ tytostr t ^ ") = (" ^
                                   exptostr e ^ ")"
               | TYFN (name, e) => "tyfn(" ^ name ^ ") = (" ^ exptostr e ^ ")"
               | APPLY (e1, e2) => exptostr e1 ^ "(" ^ exptostr e2 ^ ")"
               | TYAPPLY (e, t) => exptostr e ^ "[" ^ tytostr t ^ "]"
               | LET (var, t, e1, body) => "let " ^ var ^ ":" ^ tytostr t ^
                                           " be " ^ exptostr e1 ^ " in " ^
                                           exptostr body)
      fun decltostr (dec: decl) =
            (case dec
              of VALDECL (name, e) => "val " ^ name ^ " = " ^ exptostr e ^ "\n"
               | TYDECL (name, arg, t) => "type " ^ name ^
                                          (case arg
                                             of NONE => ""
                                              | SOME s => "[" ^ name ^ "]") ^
                                          " = " ^ tytostr t ^ "\n"
               )
      fun modexptostr (modexpr: modexp) =
            (case modexpr
              of MOD (decls) => "mod " ^ (foldr(fn(a:decl, b)=>decltostr a^b) ""
                                               decls) ^ "end\n"
               | MVAR (name) => "mod " ^ name
              )
  in (case decls of [] => ""
                  | _ => (foldr (fn(a:decl, b) => decltostr a ^ b) "" decls) ^
                         "\n\n") ^
     exptostr e ^ "\n"
  end

end
