(* syntax.sml *)

(** Abstract syntax datatypes **)

structure Syntax =
struct

datatype operator = PLUS | TIMES | MINUS | EQUAL | LESS


datatype ty = INT
            | BOOL
            | FUNCTION of ty * ty
            | TYVAR of string
            | POLY of string * ty

datatype exp = VAR of string
             | NUM of int
             | PRIM of (operator * exp * exp) (* Binary ops only *)
             | TRUE
             | FALSE
             | IF of (ty * exp * exp *exp) (* if [ty] exp1 then exp2 else exp3*)
             | FN of (string * ty * ty * exp)
             | TYFN of (string * exp)
             | APPLY of (exp * exp)
             | TYAPPLY of (exp * ty)
             | LET of (string * ty * exp * exp) (* x:ty=exp1 in exp2 *)

datatype decl = TYDECL of string * string option * ty (* type foo ['a] = ty *)
              | VALDECL of string * ty option * exp   (* val foo (:ty) = exp *)
              | FUNDECL of string * string * ty * ty * exp
              | TYFUNDECL of string * string * ty * exp

type prog = decl list * exp

end
