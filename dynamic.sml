(* dynamic.sml *) 
(* evaluator *)

signature EVAL =
sig
  datatype value
  = Num of int
  | Bool of bool
  | Closure of Ast.exp * value Env.env
  val eval : Ast.prog -> value
  val valOf : value Env.env * Ast.exp -> value
  exception RunError of string
end

structure Eval : EVAL =
struct

local
  open Ast
  open Env
in

exception RunError of string


(* "Bottom" values *)
datatype value
  = Num of int
  | Bool of bool
  | Closure of exp * value env

(* things that you can pass to apply() as arguments: values or types *)
datatype appargval
  = TyValArg of ty
  | ValArg of value

fun valOf (env: value env, VAR x) = getval env x
  | valOf (env, PATH (modlist, var)) =
      (case modlist
         of nil => raise RunError "nil path?"
          | _ => getval (pathEnv env modlist) var)
  | valOf (env, NUM n) = Num n
  | valOf (env, PRIM(oper, e1, e2)) =
      let val v1 = valOf(env, e1)
          val v2 = valOf(env, e2)
      in (case (v1, v2)
            of (Num n1, Num n2) => (case oper of PLUS  => Num (n1 + n2)
                                               | TIMES => Num (n1 * n2)
                                               | MINUS => Num (n1 - n2)
                                               | EQUAL => Bool(n1 = n2)
                                               | LESS  => Bool(n1 < n2))
             | _ => raise RunError "bad args to primop")
      end
  | valOf (env, TRUE) = Bool true
  | valOf (env, FALSE) = Bool false
  | valOf (env, IF(exp, tt, ff)) =
      (case valOf(env, exp) of Bool true => valOf(env, tt)
                            | Bool false => valOf(env, ff)
                            | _ => raise RunError "non-bool if predicate")
  | valOf (env, f as FN _) = Closure(f, env)
  | valOf (env, f as TYFN (_, e)) = valOf(env, e)
  | valOf (env, APPLY(func, arg)) = apply(valOf(env, func), valOf(env, arg))
  | valOf (env, TYAPPLY(func, arg)) = valOf(env, func)
  | valOf (env, LET(x, _, exp, body)) =
      valOf(bindval env x (valOf(env, exp)), body)

and apply (func as Closure(FN(name, ty, e), env), arg) =
             valOf(bindval env name arg, e)
  | apply (func as Closure(TYFN(name, e), env), arg) =
             valOf(env, e)
  | apply (_, arg) = raise RunError "Trying to apply a non-function"

fun eval (program: prog as Prog(declist, expr)) =
  let fun makeEnv (decls, envir) =
      (foldl(fn (dec, env) =>
                 (case dec
                   of TYDECL (name, arg, ty) => env
                    | VALDECL (name, e) => bindval env name (valOf (env, e))
                    | MODDECL (name, modexpr) =>
                        (case modexpr
                           of MVAR mvar => bindmod env name (getmod env mvar)
                            | MOD (moddecls) =>
                                bindmod env name (makeEnv(moddecls, env)))))
            envir decls)
      val env = makeEnv (declist, newenv)
  in
     valOf (env, expr)
  end

end
end
