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
      if (eval(env, exp) = true) then eval(env, tt) else eval(env, ff)
  | eval (env, f as FUN _) = Closure(f, env)
  | eval (env, APPLY(func, arg)) = eval(env, APPLY(eval(env, func), arg))
  | eval (env, APPLY(func as Closure(FUN(f, x, _, _, e), _), arg)) =
      eval(bind(bind(env, x, eval(env, arg)), f, func), e)
  | eval (env, LET(x, _, exp, body)) =
      eval(bind(env, x, eval(env, exp)), body)

end
