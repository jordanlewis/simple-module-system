(* dynamic.sml *) 
(* evaluator *)

signature EVAL =
sig
  type environment
  type modenv
  type value
  type appargval
  val valOf : modenv * Ast.exp -> value
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
  | Closure of exp * modenv
and module = MODULE of modenv
           | POINTER of modname
withtype environment = value mapping
(* mapping from module name -> module environment: "" is local environment *)
     and modenv = environment * module mapping

(* things that you can pass to apply() as arguments: values or types *)
datatype appargval
  = TyValArg of ty
  | ValArg of value

fun valOf (env as (locenv, _), VAR x) = locenv x
  | valOf (env as (locenv, mods), PATH (modlist, var)) =
      (case modlist
         of nil => raise Fail "nil path?"
          | _ => let val rec pathEnv : (modname * modenv) -> modenv =
                       fn (mname, menv as (subenv, submods)) =>
                            (case (submods mname)
                               of MODULE env => env
                                | POINTER y => pathEnv (y, menv))
                     val (subenv, mods) = foldl pathEnv env modlist
                 in subenv var
                 end)
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
  | valOf (env as (locenv, mods), LET(x, _, exp, body)) =
      valOf((bind(locenv, x, valOf(env, exp)), mods), body)

and apply (func as Closure(FN(name, ty, e), env as (locenv, mods)), arg) =
             valOf((bind(locenv, name, arg), mods), e)
  | apply (func as Closure(TYFN(name, e), env), arg) =
             valOf(env, e)
  | apply (_, arg) = raise RunError "Trying to apply a non-function"

fun valToStr(v: value) =
  (case v
     of Num n => Int.toString n
      | Bool b => Bool.toString b
      | Closure (expr, env) => "Closure(" ^ expToStr expr ^ ", )" )

fun eval (program: prog as Prog(declist, expr)) =
  let fun makeEnv (decls, envir) =
      (foldl(fn (dec, env as (locenv, mods)) =>
                 (case dec
                   of TYDECL (name, arg, ty) => env
                    | VALDECL (name, e) => (bind(locenv, name, valOf(env, e)),
                                            mods)
                    | MODDECL (name, modexpr) =>
                        (case modexpr
                           of MVAR mvar => (locenv, bind(mods, name,
                                                         POINTER(mvar)))
                            | MOD (moddecls) =>
                                (locenv,
                                  bind(mods, name,
                                       MODULE(makeEnv (moddecls,
                                                       (empty, empty))))))))
            envir decls)
      val env = makeEnv (declist, (empty, empty))
  in
     valOf (env, expr)
  end

end
end
