open Gameutil
open Definitions

include Object with type cons = color

val setMoves : t -> (direction * direction) list -> unit
val setFocus : t -> bool -> unit

val getData : t -> team_data