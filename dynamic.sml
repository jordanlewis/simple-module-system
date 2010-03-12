(* dynamic.sml *)

(* evaluator *)

signature EVAL =
sig
  type environment
  type value
  type appargval
  val eval : environment * Syntax.exp -> value
  val apply : value * appargval -> value
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
  | Closure of exp * {venv: value mapping, tenv: ty mapping}

(* things that you can pass to apply() as arguments: values or types *)
datatype appargval
  = TyValArg of ty
  | ValArg of value

type environment = {venv: value mapping, tenv: ty mapping}

fun eval ({venv, tenv}, VAR x) = venv(x)
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
  | eval (env, IF(_, exp, tt, ff)) =
      (case eval(env, exp) of Bool true => eval(env, tt)
                            | Bool false => eval(env, ff)
                            | _ => raise RunError "non-bool if predicate")
  | eval (env, f as FN _) = Closure(f, env)
  | eval (env, APPLY(func, arg)) =
      (case arg
         of ExpArg exparg => apply(eval(env, func), ValArg (eval(env, exparg)))
          | TyArg  tyarg  => apply(eval(env, func), TyValArg tyarg))

  (*| eval (env, TYAPPLY(func, tyarg)) = tyapply(eval(env, func), tyarg)*)
  | eval (env as {venv, tenv}, LET(x, _, exp, body)) =
      eval({venv = bind(venv, x, eval(env, exp)), tenv = tenv}, body)

and apply (func as Closure(FN(VarParam(name, _), t, e), env as {venv, tenv}),
            arg as ValArg valarg) =
             eval({venv = bind(venv, name, valarg), tenv = tenv}, e)
  | apply (func as Closure(FN(TyParam(name), t, e), env as {venv, tenv}),
           arg as TyValArg tyvalarg) =
             eval({venv = venv, tenv = bind(tenv, name, tyvalarg)}, e)
  | apply (_, arg) = raise RunError "Trying to apply a non-function"


end
end
