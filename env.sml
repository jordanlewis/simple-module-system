(* env.sml *)

(*** environments ***)

structure Env =
struct

(* environments are finite mappings from strings to 'a *)


type 'a mapping = string -> 'a

type ('a,'b) environment = {tenv: 'a mapping, venv: 'b mapping}

val emptymap : 'a mapping =
      fn x => raise Fail(concat["unbound variable \"", x, "\""])

val empty = {tenv = emptymap, venv = emptymap}

(* Curried function - build new envs with bind(env, x, v) and look up by
 * applying that to the value to look up*)
fun bind (env : 'a mapping, name, value) lookup =
  if (name = lookup) then value else env(lookup)

end
