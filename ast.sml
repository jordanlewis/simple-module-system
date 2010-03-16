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
            | TYVARARG of tyname
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


end
