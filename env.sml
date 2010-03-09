(* env.sml *)

(*** environments ***)

structure Env =
struct

(* environments are finite mappings from strings to 'a *)

type 'a mapping = string -> 'a

val empty : 'a mapping =
      fn x => raise Fail(concat["unbound variable \"", x, "\""])

(* Curried function - build new envs with bind(env, x, v) and look up by
 * applying that to the value to look up*)
fun bind (env : 'a mapping, name, value) lookup =
  if (name = lookup) then value else env(lookup)

end
