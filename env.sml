(* env.sml *)

(*** environments ***)

structure Env =
struct

(* environments are finite mappings from strings to 'a *)


type 'a mapping = string -> 'a

datatype 'a env = ENV of 'a mapping * ('a env) mapping
                | ENVVAR of string

val empty : 'a mapping =
      fn x => raise Fail(concat["unbound variable \"", x, "\""])

(* Curried function - build new envs with bind(env, x, v) and look up by
 * applying that to the value to look up*)
fun bind (map : 'a mapping, name, value) lookup =
  if (name = lookup) then value else map(lookup)

fun bindname (e: 'a env) (name: string) (value: 'a) =
  case e
    of ENV (map, subenv) => ENV(bind(map, name, value), subenv)
     | ENVVAR s => raise Fail "var bindname"

fun bindmod (e: 'a env) (name: string) (newenv: 'a env) : 'a env=
  case e
    of ENV (map, subenv) => ENV(map, bind(subenv, name, newenv))
     | ENVVAR s => raise Fail "var bindmod"

fun lookup (e: 'a env) (name: string) : 'a =
  case e
    of ENV (map, subenv) => map name
     | ENVVAR s => raise Fail "var lookup"


fun pathEnv (e: 'a env) (path: string list) =
  let fun subEnv (name: string, e: 'a env) =
        case e
          of ENV (map, subenv) =>
            let val x = subenv name
            in case x of ENV _ => x
                       | ENVVAR s => subEnv(s, e)
            end
           | ENVVAR s => raise Fail "var pathname 1"
  in
    foldl subEnv e path
  end
end
