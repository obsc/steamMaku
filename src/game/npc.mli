open Definitions
open Gameutil

include Object with type cons = unit

val create : unit -> t

val spawn : npctype -> t -> unit 
val update : t -> unit
val hit : t -> int -> color -> bool