(* dynamic.sml *)

(* evaluator *)

signature EVAL =
sig
  type environment
  type value
  type appargval
  val eval : environment * Syntax.exp -> value
  val apply : value * value -> value
end

structure Eval : EVAL =
struct

local
  open Syntax
  open Env
in

exception RunError of string


(* "Bottom" values *)
datatype value
  = Num of int
  | Bool of bool
  | Closure of exp * value mapping

type environment = value mapping

(* things that you can pass to apply() as arguments: values or types *)
datatype appargval
  = TyValArg of ty
  | ValArg of value

fun eval (env: value mapping, VAR x) = env(x)
  | eval (env, NUM n) = Num n
  | eval (env, PRIM(oper, e1, e2)) =
      let val v1 = eval(env, e1)
          val v2 = eval(env, e2)
      in (case (v1, v2)
            of (Num n1, Num n2) => (case oper of PLUS  => Num (n1 + n2)
                                               | TIMES => Num (n1 * n2)
                                               | MINUS => Num (n1 - n2)
                                               | EQUAL => Bool(n1 = n2)
                                               | LESS  => Bool(n1 < n2))
             | _ => raise RunError "bad args to primop")
      end
  | eval (env, TRUE) = Bool true
  | eval (env, FALSE) = Bool false
  | eval (env, IF(exp, tt, ff)) =
      (case eval(env, exp) of Bool true => eval(env, tt)
                            | Bool false => eval(env, ff)
                            | _ => raise RunError "non-bool if predicate")
  | eval (env, f as FN _) = Closure(f, env)
  | eval (env, f as TYFN (_, e)) = eval(env, e)
  | eval (env, APPLY(func, arg)) = apply(eval(env, func), eval(env, arg))
  | eval (env, TYAPPLY(func, arg)) = eval(env, func)
  | eval (env, LET(x, _, exp, body)) =
      eval(bind(env, x, eval(env, exp)), body)

and apply (func as Closure(FN(name, ty, e), env), arg) =
             eval(bind(env, name, arg), e)
  | apply (func as Closure(TYFN(name, e), env), arg) =
             eval(env, e)
  | apply (_, arg) = raise RunError "Trying to apply a non-function"


end
end
