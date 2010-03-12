(* syntax.sml *)

(** Abstract syntax datatypes **)

structure Syntax =
struct

datatype operator = PLUS | TIMES | MINUS | EQUAL | LESS


type varname = string
type tyname = string

datatype ty = INT
            | BOOL
            | FUNCTION of ty * ty
            | TYVAR of tyname
            | POLY of tyname * ty

datatype param = VarParam of varname * ty
               | TyParam of tyname

datatype exp = VAR of varname
             | NUM of int
             | PRIM of (operator * exp * exp) (* Binary ops only *)
             | TRUE
             | FALSE
             | IF of (ty * exp * exp *exp) (* if [ty] exp1 then exp2 else exp3*)
             | FN of param * ty * exp
             | APPLY of (exp * apparg)
             | LET of (string * ty * exp * exp) (* x:ty=exp1 in exp2 *)

and apparg = ExpArg of exp
           | TyArg of ty

datatype decl = TYDECL of string * string option * ty (* type foo ['a] = ty *)
              | VALDECL of string * ty option * exp   (* val foo (:ty) = exp *)
              | FUNDECL of string * string * ty * ty * exp
              | TYFUNDECL of string * string * ty * exp

datatype prog = SimpleProg of exp
              | Prog of decl list * exp

end
