open Definitions
open Gameutil

include Object with type cons = Player.t * Player.t

(* Spawns a single ufo *)
val spawn : t -> unit

(* Event upon hitting a ufo with a bullet *)
val hit : t -> int -> color -> bool

(* Hitboxes of the npcs *)
val getHitbox : t -> (id * hitbox) list

(* Returns the internal data for npcs *)
val getData : t -> ufo list

(* Sets a reference to powerups*)
val setSpawn : t -> (position -> color -> bullet_type
               -> acceleration -> position -> unit)-> unit