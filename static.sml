(* static type checker *)

signature TYPECHECK =
sig
  type modenv
  val replacety : (Ast.tyname * Ast.ty * Ast.ty) -> Ast.ty
  val typeOf : modenv * Ast.exp -> Ast.ty
end

structure TypeCheck =
struct

local
  open Ast
  open Env
in

type environment = ty mapping
datatype module = MODULE of modenv
                | POINTER of modname
(* mapping from module name -> module environment: "" is local environment *)
withtype modenv = environment * module mapping

exception TypeError of string

fun replacety (find, replace, ty as TYVAR x) =
      if (find = x) then replace else ty
  | replacety (find, replace, FUNCTION (t1, t2)) =
      FUNCTION(replacety(find, replace, t1), replacety(find, replace, t2))
  | replacety (find, replace, POLY (x, t)) =
      POLY(x, replacety(find, replace, t))
  | replacety (find, replace, ty) = ty

fun typeOf (env as (locenv, mods), VAR x) = locenv x
  | typeOf (env:modenv as (locenv, mods), PATH (modlist, var)) =
      (case modlist
         of nil => raise TypeError "nil path?"
          | m::nil => (case (mods m)
                             of MODULE (x as (subenv, mods)) => subenv var
                              | POINTER y => raise Fail "modpointer")
          | _ => let val pathEnv : (modname * modenv) -> modenv =
                       fn (mname, menv as (subenv, submods)) =>
                            (case (submods mname)
                               of POINTER y => raise Fail "pointer"
                               | MODULE env => env)
                     val (subenv, mods) = foldl pathEnv env modlist
                 in subenv var
                 end)
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
  | typeOf (env as (locenv, mods), FN(name, ty, exp)) =
      FUNCTION(ty, typeOf((bind(locenv, name, ty), mods), exp))
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
  | typeOf (env as (locenv, mods),  LET(x, ty, exp, body)) = 
      if typeOf(env, exp) = ty
      (* add x => ty to new env *)
      then typeOf((bind(locenv, x, ty), mods), body)
      else raise TypeError "type mismatch in let"

fun typeCheck (program: prog as Prog(declist, expr)) =
  let fun makeEnv (decls, envir) =
      (foldl(fn (dec, env as (locenv, mods)) =>
                 (case dec
                   of TYDECL (name, arg, ty) => (bind(locenv, name, ty), mods)
                    | VALDECL (name, e) => (bind(locenv, name, typeOf(env, e)),
                                            mods)
                    | MODDECL (name, modexpr) =>
                        (case modexpr
                           of MVAR mvar => raise TypeError "mvar"
                            | MOD (moddecls) =>
                                (locenv,
                                  bind(mods, name,
                                       MODULE(makeEnv (moddecls,
                                                       (empty, empty))))))))
            envir decls)
      val env = makeEnv (declist, (empty, empty))
  in
    typeOf (env, expr)
  end

end
end
