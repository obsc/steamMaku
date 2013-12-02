open Gameutil
open Definitions

include Object with type cons = Npc.t

(* Spawns a single type of powerup *)
val spawn : t -> Player.t -> bullet_type -> acceleration -> position -> unit

(* Collision handling *)
val collideAll : t -> unit

(* Returns internal data for powerups *)
val getData : t -> bullet list