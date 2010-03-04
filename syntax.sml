(* syntax.sml *)

(** Abstract syntax datatypes **)

structure Syntax =
struct

datatype operator = PLUS | TIMES | MINUS | EQUAL | LESS

datatype ty = INT
            | BOOL
            | FUNCTION of ty * ty
            | TYVAR of string

datatype exp = VAR of string
             | NUM of int
             | PRIM of (operator * exp * exp) (* Binary ops only *)
             | TRUE
             | FALSE
             | IF of (ty * exp * exp *exp) (* if [ty] exp1 then exp2 else exp3*)
             | FUN of (string * string * ty *exp) 
             (*        name,    argname, type, body *)
             | APPLY of (exp * exp)
             | LET of (string * ty * exp * exp) (* x:ty=exp1 in exp2 *)
end
