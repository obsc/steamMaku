open Gameutil
open Definitions
open Util
open Netgraphics

module type Collider = sig
  (* Player color -> bullet color -> can collide *)
  val collide : color -> color -> bool
  val graze : color -> color -> bool
  (* val collideNpc : color -> bool *)

  (* Events upon collision *)
  val playerEvent : Player.t -> bool
  val grazeEvent : Player.t -> bool
  (* val npcEvent : color -> Npc.t -> unit *)

  (* Spawn a list of bullets/powers *)
  val spawn : Player.t -> bullet_type -> acceleration
              -> position -> bullet list
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

  let isCollide (p1, r1 : hitbox) (p2, r2 : hitbox) : bool =
    distance p1 p2 < r1 +. r2

  let updatePos (a : bullet list) (x : bullet) : bullet list =
    let pos : position = add_v x.b_pos x.b_vel in
    let vel : velocity = add_v x.b_vel x.b_accel in
    if in_bounds pos then begin
      add_update (MoveBullet (x.b_id, pos));
      { x with b_pos = pos; b_vel = vel }::a
    end
    else begin
      add_update (DeleteBullet x.b_id); a
    end

  let getHitbox (x : bullet) : hitbox =
    (x.b_pos, float_of_int x.b_radius)

  let checkPlayer b p : bool =
    if C.collide (Player.getColor p) b.b_color &&
      isCollide (getHitbox b) (Player.getHitbox p) then C.playerEvent p
    else if C.graze (Player.getColor p) b.b_color &&
      isCollide (getHitbox b) (Player.getGrazebox p) then C.grazeEvent p
    else false

  let collideOne red blue (a : bullet list) (x : bullet) : bullet list =
    if checkPlayer x red || checkPlayer x blue
    then (add_update (DeleteBullet x.b_id); a) else x::a

  let addBullet (a : bullet list) (x : bullet) : bullet list =
    add_update (AddBullet (x.b_id, x.b_color, x.b_type, x.b_pos));
    x::a

  let create (red, blue : cons) : t = (ref [], red, blue)
  let spawn (x : t) player b_type accel pos : unit =
    if Player.reduceCharge player (cost_of_bullet b_type) then begin
      let new_b : bullet list = C.spawn player b_type accel pos in
      match x with (b_lst, r, b) ->
      b_lst := (List.fold_left addBullet !b_lst new_b)
    end else ()

  let update (x : t) : unit =
    match x with (b_lst, r, b) ->
    b_lst := (List.fold_left updatePos [] !b_lst)
  
  let collideAll (x : t) : unit =
    match x with (b_lst, r, b) ->
    b_lst := (List.fold_left (collideOne r b) [] !b_lst)

  let getData (x : t) : bullet list =
    match x with (b_lst, r, b) -> !b_lst
end