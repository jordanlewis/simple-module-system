(* static type checker *)

signature TYPECHECK =
sig
  val replacety : (Ast.tyname * Ast.ty * Ast.ty) -> Ast.ty
  val typeOf : Ast.ty Env.env * Ast.exp -> Ast.ty
end

structure TypeCheck =
struct

local
  open Ast
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

fun baseTy (env, t) =
  (case t
     of TYVAR n => baseTy (env, lookup env n)
      | TYPATH (modlist, n) =>
          let val newenv = pathEnv env modlist
          in baseTy (newenv, lookup newenv n)
          end
      | _ => t)

fun typeOf (env: ty env, VAR x) = lookup env x
  | typeOf (env, PATH (modlist, var)) =
      (case modlist
         of nil => raise TypeError "nil path?"
          | _ => lookup (pathEnv env modlist) var)
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
      FUNCTION(ty, typeOf(bindname env name ty, exp))
  | typeOf (env, TYFN(name, exp)) = POLY(name, typeOf(env, exp))
  | typeOf (env, APPLY(func, arg)) =
      (case typeOf(env, func)
         of FUNCTION(tyin, tyout) =>
             if typeOf(env, arg) = tyin then tyout
             else raise TypeError "apply argument has wrong type"
          | _ => raise TypeError "applying a non-fn")
  | typeOf (env, TYAPPLY(func, tyarg)) =
      (case typeOf(env, func)
         of POLY(name, tyout) => replacety(name, baseTy (env, tyarg), tyout)
          | _ => raise TypeError "tyapplying a non-poly")
  | typeOf (env,  LET(x, ty, exp, body)) =
      if typeOf(env, exp) = ty
      (* add x => ty to new env *)
      then typeOf(bindname env x ty, body)
      else raise TypeError "type mismatch in let"

fun typeCheck (program: prog as Prog(declist, expr)) =
  let fun makeEnv (decls : decl list, envir : ty env) : ty env =
      (foldl(fn (dec, env) =>
                 (case dec
                   of TYDECL (name, arg, ty) => bindname env name ty
                    | VALDECL (name, e) => bindname env name (typeOf(env, e))
                    | MODDECL (name, modexpr) =>
                        (case modexpr
                           of MVAR mvar => bindmod env name (ENVVAR(mvar))
                            | MOD (moddecls) =>
                                bindmod env name
                                        (makeEnv(moddecls,
                                                     ENV(empty, empty))))))
            envir decls)
      val env = makeEnv (declist, ENV(empty, empty))
  in
    typeOf (env, expr)
  end

end
end
