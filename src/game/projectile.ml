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
  (* Clear, self, enemy *)
  val playerEvent : (unit -> unit) -> Player.t -> Player.t -> bool
  val grazeEvent : Player.t -> bool
  (* val npcEvent : color -> Npc.t -> unit *)

  (* Spawn a list of bullets/powers *)
  val spawn : Player.t -> bullet_type -> acceleration
              -> position -> bullet list
end

module type MakeType = functor (C : Collider) -> sig
  type t
  type cons = Player.t * Player.t(*  * Npc.t *)

  val updateClear : t -> unit

  val create : cons -> t
  val spawn : t -> Player.t -> bullet_type -> acceleration -> position -> unit

  val update : t -> unit
  val collideAll : t -> unit

  val getData : t -> bullet list
end

module Make : MakeType = functor (C : Collider) -> struct
  type t = bullet list ref * bool ref * Player.t * Player.t(*  * Npc.t *)
  type cons = Player.t * Player.t(*  * Npc.t *)

  let setClear (x : t) () : unit =
    match x with (b_lst, clear, r, b) -> clear := true

  let updateClear (x : t) : unit =
    match x with (b_lst, clear, r, b) ->
    if !clear then begin
      List.iter (fun x -> add_update (DeleteBullet x.b_id)) !b_lst;
      b_lst := [];
      clear := false
    end else ()

  let isCollide (p1, r1 : hitbox) (p2, r2 : hitbox) : bool =
    distance p1 p2 < r1 +. r2

  let updatePos (a : bullet list) (b : bullet) : bullet list =
    let pos : position = add_v b.b_pos b.b_vel in
    let vel : velocity = add_v b.b_vel b.b_accel in
    if in_bounds pos then begin
      add_update (MoveBullet (b.b_id, pos));
      { b with b_pos = pos; b_vel = vel }::a
    end
    else begin
      add_update (DeleteBullet b.b_id); a
    end

  let getHitbox (b : bullet) : hitbox =
    (b.b_pos, float_of_int b.b_radius)

  let checkPlayer (x : t) (b : bullet) (p : Player.t) (e : Player.t) : bool =
    if C.collide (Player.getColor p) b.b_color &&
      isCollide (getHitbox b) (Player.getHitbox p)
      then C.playerEvent (setClear x) p e
    else if C.graze (Player.getColor p) b.b_color &&
      isCollide (getHitbox b) (Player.getGrazebox p)
      then C.grazeEvent p
    else false

  let collideOne (x : t) red blue (a : bullet list) (b : bullet) : bullet list =
    if checkPlayer x b red blue || checkPlayer x b blue red
    then (add_update (DeleteBullet b.b_id); a) else b::a

  let addBullet (a : bullet list) (b : bullet) : bullet list =
    add_update (AddBullet (b.b_id, b.b_color, b.b_type, b.b_pos));
    b::a

  let create (red, blue : cons) : t = (ref [], ref false, red, blue)
  let spawn (x : t) player b_type accel pos : unit =
    if Player.reduceCharge player (cost_of_bullet b_type) then begin
      let new_b : bullet list = C.spawn player b_type accel pos in
      match x with (b_lst, clear, r, b) ->
      b_lst := (List.fold_left addBullet !b_lst new_b)
    end else ()

  let update (x : t) : unit =
    match x with (b_lst, clear, r, b) ->
    b_lst := (List.fold_left updatePos [] !b_lst)
  
  let collideAll (x : t) : unit =
    match x with (b_lst, clear, r, b) ->
    b_lst := (List.fold_left (collideOne x r b) [] !b_lst)

  let getData (x : t) : bullet list =
    match x with (b_lst, clear, r, b) -> !b_lst
end