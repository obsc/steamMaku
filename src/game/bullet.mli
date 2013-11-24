open Gameutil
open Definitions

include Object with type cons = Player.t * Player.t(*  * Npc.t *)

val spawn : t -> Player.t -> bullet_type -> acceleration -> position -> unit

val collideAll : t -> unit
val updateClear : t -> unit

val getData : t -> bullet list