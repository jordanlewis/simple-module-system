(* static.sml *)

(* static type checker*)

signature TYPECHECK =
sig
  type environment
  val replacety : (Ast.tyname * Ast.ty * Ast.ty) -> Ast.ty
  val typeOf : environment * Ast.exp -> Ast.ty
end

structure TypeCheck =
struct

local
  open Ast
  open Env
in

type environment = ty mapping
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
  | typeOf (env, IF(exp, tt, ff)) =
      if (typeOf(env, exp) = BOOL) then
        (let val ty1 = typeOf(env, tt) in
           if typeOf(env, ff) = ty1 then ty1 else raise TypeError "if 2"
         end)
      else raise TypeError "if 1"
  | typeOf (env, FN(name, ty, exp)) =
      FUNCTION(ty, typeOf(bind(env, name, ty), exp))
  | typeOf (env, TYFN(name, exp)) = POLY(name, typeOf(env, exp))
  | typeOf (env, APPLY(func, arg)) =
      (case typeOf(env, func)
         of FUNCTION(tyin, tyout) =>
             if typeOf(env, arg) = tyin then tyout
             else raise TypeError "apply argument has wrong type"
          | _ => raise TypeError "applying a non-fn")
  | typeOf (env, TYAPPLY(func, tyarg)) =
      (case typeOf(env, func)
         of POLY(name, tyout) => replacety(name, tyarg, tyout)
          | _ => raise TypeError "tyapplying a non-poly")
  | typeOf (env, LET(x, ty, exp, body)) = 
      if typeOf(env, exp) = ty
      (* add x => ty to new env *)
      then typeOf(bind(env, x, ty), body)
      else raise TypeError "type mismatch in let"

fun typeCheck (program: prog as Prog(decls, expr)) =
  let val env = 
    foldl(fn (dec: decl, env) =>
              (case dec
                 of TYDECL (name, arg, ty) => bind(env, name, ty)
                  | VALDECL (name, e) => bind(env, name, typeOf(env, e))
                  | _ => env))
          empty decls
  in
    typeOf (env, expr)
  end

end
end
