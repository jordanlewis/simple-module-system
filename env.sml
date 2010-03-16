(* env.sml *)

(*** environments ***)

structure Env =
struct

(* environments are finite mappings from strings to 'a *)


type 'a mapping = string -> 'a

datatype 'a env = ENV of {venv: 'a mapping, tenv: 'a mapping,
                          menv: 'a env mapping, path: string list}

val empty : 'a mapping =
      fn x => raise Fail(concat["unbound variable \"", x, "\""])

val newenv = ENV {venv=empty, tenv=empty, menv=empty, path=[]}

(* Curried function - build new envs with bind(env, x, v) and look up by
 * applying that to the value to look up*)
fun bind (map : 'a mapping, name, value) lookup =
  if (name = lookup) then value else map(lookup)

fun bindval (e as ENV {venv=v, tenv=t, menv=m,path=p}) (name: string)
            (value: 'a) =
  ENV {venv=bind(v, name, value), tenv=t, menv=m, path=p}

fun bindty (e as ENV {venv=v, tenv=t, menv=m,path=p}) (name: string) (typ: 'a) =
  ENV {venv=v, tenv=bind(t, name, typ), menv=m, path=p}

fun bindmod (e as ENV {venv=v, tenv=t, menv=m, path=p}) (name: string)
            (newenv: 'a env) =
  ENV {venv=v, tenv=t, menv=bind(m, name, newenv), path=p}

fun appendpath (e as ENV {venv=v, tenv=t, menv=m, path=p}, name: string) =
  ENV {venv=v, tenv=t, menv=m, path=name::p}

fun bindpath (e as ENV {venv=v, tenv=t, menv=m, path=p}, path: string list) =
  ENV {venv=v, tenv=t, menv=m, path=p}


fun getval (e as ENV {venv=v, ...}) (name: string) = v name
fun getty (e as ENV {tenv=t, ...}) (name: string) = t name
fun getmod (e as ENV {menv=m, ...}) (name: string) = m name

fun pathEnv (e: 'a env) (path: string list) =
  let fun subEnv (name:string, e) =
    getmod e name
  in
    bindpath (foldl subEnv e path, path)
  end
end
