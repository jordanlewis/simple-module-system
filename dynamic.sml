structure Eval =
struct

open Syntax
open Env

datatype value
  = Num of int
  | Bool of bool
  | Closure of exp * value mapping
type environment = value mapping

fun eval (env, VAR x) = env(x)
  | eval (env, NUM n) = Num n
  (*
  | eval (env, PRIM(op, e1, e2)) =
      let val v1 as Num n1 = eval(e1)
          val v2 as Num n2 = eval(e2)
      in (case op of PLUS  => n1 + n2
                   | TIMES => n1 * n2
                   | MINUS => n1 - n2
                   | EQUAL => n1 = n2
                   | LESS  => n1 < n2)
      end
      *)
  | eval (env, TRUE) = Bool true
  | eval (env, FALSE) = Bool false
  | eval (env, IF(_, exp, tt, ff)) =
      (case eval(env, exp) of Bool true => eval(env, tt)
                            | Bool false => eval(env, ff))
  | eval (env, f as FUN _) = Closure(f, env)
  | eval (env, APPLY(func, arg)) = apply(eval(env, func), eval(env, arg))
  | eval (env, LET(x, _, exp, body)) =
      eval(bind(env, x, eval(env, exp)), body)

and apply (func as Closure(FUN(f, x, _, _, e), env), arg : value) =
      eval(bind(bind(env, x, arg), f, func), e)

end
