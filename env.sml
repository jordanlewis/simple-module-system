(* env.sml *)

(*** environments ***)

structure Env =
struct

(* environments are finite mappings from strings to 'a *)


type 'a mapping = string -> 'a

datatype 'a env = ENV of {venv: 'a mapping, tenv: 'a mapping,
                          menv: 'a env mapping}

val empty : 'a mapping =
      fn x => raise Fail(concat["unbound variable \"", x, "\""])

val newenv = ENV {venv=empty, tenv=empty, menv=empty}

(* Curried function - build new envs with bind(env, x, v) and look up by
 * applying that to the value to look up*)
fun bind (map : 'a mapping, name, value) lookup =
  if (name = lookup) then value else map(lookup)

fun bindval (e as ENV {venv=v, tenv=t, menv=m}) (name: string) (value: 'a) =
  ENV {venv=bind(v, name, value), tenv=t, menv=m}

fun bindty (e as ENV {venv=v, tenv=t, menv=m}) (name: string) (typ: 'a) =
  ENV {venv=v, tenv=bind(t, name, typ), menv=m}

fun bindmod (e as ENV {venv=v, tenv=t, menv=m}) (name: string)
            (newenv: 'a env) =
  ENV {venv=v, tenv=t, menv=bind(m, name, newenv)}


fun getval (e as ENV {venv=v, ...}) (name: string) = v name
fun getty (e as ENV {tenv=t, ...}) (name: string) = t name
fun getmod (e as ENV {menv=m, ...}) (name: string) = m name

fun pathEnv (e: 'a env) (path: string list) =
  let fun subEnv (name:string, e) =
    getmod e name
  in
    foldl subEnv e path
  end
end
