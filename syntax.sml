(* syntax.sml *)

(** Abstract syntax datatypes **)

structure Syntax =
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
             | IF of (exp * exp *exp) (* if exp1 then exp2 else exp3*)
             | FN of varname * ty * exp
             | TYFN of tyname * exp
             | APPLY of exp * exp
             | TYAPPLY of exp * ty
             | LET of (string * ty * exp * exp) (* x:ty=exp1 in exp2 *)

datatype decl = TYDECL of string * string option * ty (* type foo ['a] = ty *)
              | VALDECL of string * exp   (* val foo = exp *)
              | MODDECL of string * modexp

and modexp = MOD of decl list
           | MVAR of modname


datatype prog = Prog of decl list * exp

end
