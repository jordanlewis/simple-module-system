(* env.sml *)

(*** environments ***)

structure Env =
struct

type 'a environment = (string * 'a) list

val empty : 'a environment = nil

fun bind (env : 'a environment, name, value) 
