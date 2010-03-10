(* static.sml *)

(* static type checker*)

structure TypeCheck =
struct

local
  open Syntax
  open Env
in

exception TypeError of string

fun replacety (find, replace, ty as TYVAR x) =
      if (find = x) then replace else ty
  | replacety (find, replace, FUNCTION (t1, t2)) =
      FUNCTION(replacety(find, replace, t1), replacety(find, replace, t2))
  | replacety (find, replace, POLY (x, t)) =
      POLY(x, replacety(find, replace, t))
  | replacety (find, replace, ty) = ty

fun typeOf (env, VAR x) = env x
  | typeOf (env, NUM n) = INT
  | typeOf (env, PRIM(PLUS, e1, e2)) = INT
  | typeOf (env, PRIM(TIMES, e1, e2)) = INT
  | typeOf (env, PRIM(MINUS, e1, e2)) = INT
  | typeOf (env, PRIM(EQUAL, e1, e2)) = BOOL
  | typeOf (env, PRIM(LESS, e1, e2)) = BOOL
  | typeOf (env, TRUE) = BOOL
  | typeOf (env, FALSE) = BOOL
  | typeOf (env, IF(ty, exp, tt, ff)) =
      if (typeOf(env, exp) = BOOL) andalso
         (typeOf(env, tt) = ty) andalso
         (typeOf(env, ff) = ty)
      then ty
      else raise TypeError "if"
  | typeOf (env, FN(x, tyin, tyout, exp)) =
      (* new environment with x bound *)
      if typeOf(bind(env, x, tyin), exp) = tyout
      then FUNCTION(tyin, tyout)
      else raise TypeError "fun"
  | typeOf (env, TYFN(t, exp)) = POLY(t, typeOf(env, exp))
  | typeOf (env, APPLY(func, arg)) =
      (case typeOf(env, func)
         of FUNCTION(tyin, tyout) => if typeOf(env, arg) = tyin
                                    then tyout
                                    else raise TypeError "domain"
         | _ => raise TypeError "applying non-function")
  | typeOf (env, TYAPPLY(func, arg)) =
      (case typeOf(env, func)
        of POLY(t, tyout) => replacety(t, arg, tyout)
        | _ => raise TypeError "applying non-polyfunction")
  | typeOf (env, LET(x, ty, exp, body)) = 
      if typeOf(env, exp) = ty
      (* add x => ty to new env *)
      then typeOf(bind(env, x, ty), body)
      else raise TypeError "type mismatch in let"
end
end
