open Gameutil
open Definitions

include Object with type cons = color

(* Player input action handlers *)
val setMoves : t -> (direction * direction) list -> unit
val setFocus : t -> bool -> unit

(* Updates player charge *)
val updateCharge : t -> unit

(* Reduces the amount of charge that a player has *)
val reduceCharge : t -> int -> bool

(* Gets the state of the player *)
val getHitbox : t -> hitbox
val getPos : t -> position
val getColor : t -> color
val getScore : t -> int

(* Returns team data *)
val getData : t -> team_data