open Definitions
open Gameutil

include Object with type cons = unit

val spawn : npctype -> t -> unit
val hit : t -> int -> color -> bool