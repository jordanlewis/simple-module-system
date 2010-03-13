(* dynamic.sml *)

(* evaluator *)

signature EVAL =
sig
  type environment
  type value
  type appargval
  val valOf : environment * Ast.exp -> value
  val apply : value * value -> value
  val eval : Ast.prog -> value
  val valToStr : value -> string
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
  | Closure of exp * value mapping

type environment = value mapping

(* things that you can pass to apply() as arguments: values or types *)
datatype appargval
  = TyValArg of ty
  | ValArg of value

fun valOf (env: value mapping, VAR x) = env(x)
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
      valOf(bind(env, x, valOf(env, exp)), body)

and apply (func as Closure(FN(name, ty, e), env), arg) =
             valOf(bind(env, name, arg), e)
  | apply (func as Closure(TYFN(name, e), env), arg) =
             valOf(env, e)
  | apply (_, arg) = raise RunError "Trying to apply a non-function"

fun valToStr(v: value) =
  (case v
     of Num n => Int.toString n
      | Bool b => Bool.toString b
      | Closure (expr, env) => "Closure(" ^ expToStr expr ^ ", )" )

fun eval (program: prog as Prog(decls, expr)) =
  let val env =
    foldl(fn (dec: decl, env) =>
              (case dec
                 of VALDECL (name, e) => bind(env, name, valOf(env, e))
                  | _ => env))
          empty decls
  in
    valOf (env, expr)
  end


end
end
