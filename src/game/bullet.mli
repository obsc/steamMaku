open Gameutil
open Definitions

include Object with type cons = Player.t * Player.t(*  * Npc.t *)

val collideAll : t -> unit

val getData : t -> bullet list