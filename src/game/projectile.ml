open Gameutil
open Definitions
open Util
open Netgraphics

module type Collider = sig
  val collideRed : color -> bool
  val collideBlue : color -> bool
  (* val collideNpc : color -> bool *)

  val pEvent : color -> Player.t -> unit
  (* val npcEvent : color -> Npc.t -> unit *)

  val spawn : Player.t -> bullet_type -> acceleration
              -> position -> bullet list -> bullet list
end

module type MakeType = functor (C : Collider) -> sig
  type t
  type cons = Player.t * Player.t(*  * Npc.t *)

  val create : cons -> t
  val spawn : t -> Player.t -> bullet_type -> acceleration -> position -> unit

  val update : t -> unit
  val collideAll : t -> unit

  val getData : t -> bullet list
end

module Make : MakeType = functor (C : Collider) -> struct
  type t = bullet list ref * Player.t * Player.t(*  * Npc.t *)
  type cons = Player.t * Player.t(*  * Npc.t *)

  let is_collide (p1, r1 : hitbox) (p2, r2 : hitbox) : bool =
    distance p1 p2 < r1 +. r2

  let update_pos (a : bullet list) (x : bullet) : bullet list =
    let pos : position = add_v x.b_pos x.b_vel in
    let vel : velocity = add_v x.b_vel x.b_accel in
    if in_bounds pos then begin
      add_update (MoveBullet (x.b_id, pos));
      { x with b_pos = pos; b_vel = vel }::a
    end
    else begin
      add_update (DeleteBullet x.b_id); a
    end

  let create (red, blue : cons) : t = (ref [], red, blue)
  let spawn (x : t) player b_type accel pos : unit =
    match x with (b_lst, r, b) ->
    b_lst := (C.spawn player b_type accel pos !b_lst)

  let update (x : t) : unit =
    match x with (b_lst, r, b) ->
    b_lst := (List.fold_left update_pos [] !b_lst)
  let collideAll (x : t) : unit = ()

  let getData (x : t) : bullet list =
    match x with (b_lst, r, b) -> !b_lst
end