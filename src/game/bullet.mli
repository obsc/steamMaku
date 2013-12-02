open Gameutil
open Definitions

include Object with type cons = Player.t * Player.t(*  * Npc.t *)

(* Spawns a single type of bullet *)
val spawn : t -> Player.t -> bullet_type -> acceleration -> position -> unit

(* Collision handling *)
val collideAll : t -> unit

(* Clears all bullets *)
val clearAll : t -> unit

(* Update event for clearing all bullets *)
val updateClear : t -> unit

(* Returns internal data for bullets *)
val getData : t -> bullet list