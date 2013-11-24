open Gameutil
open Definitions
open Util

module type Collider = sig
  type t
  type out

  val collideRed : color -> bool
  val collideBlue : color -> bool
  (* val collideNpc : color -> bool *)

  val pEvent : color -> Player.t -> unit
  (* val npcEvent : color -> Npc.t -> unit *)

  val toData : t -> out
end

module type MakeType = functor (C : Collider) -> sig
  type t = C.t
  type cons = Player.t * Player.t(*  * Npc.t *)

  val create : cons -> t
  val update : t -> unit

  val collideAll : t -> unit
  val getData : t -> C.out
end

module Make : MakeType = functor (C : Collider) -> struct
  type t = C.t
  type cons = Player.t * Player.t(*  * Npc.t *)

  let is_collide (p1, r1 : hitbox) (p2, r2 : hitbox) : bool =
    distance p1 p2 < r1 +. r2

  let create (red, blue : cons) : t = failwith "todo"
  let update (x : t) : unit = failwith "todo"

  let collideAll (x : t) : unit = failwith "todo"

  let getData (x : t) : C.out = failwith "todo"
end