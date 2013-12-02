open Gameutil
open Definitions
open Util
open Netgraphics

module type Collider = sig
  (* Player color -> bullet color -> can collide *)
  val collide : color -> color -> bool
  val graze : color -> color -> bool
  (* val collideNpc : color -> bool *)

  (* Events upon collision
   * the boolean returned determines if the projectile is deleted or not
   * clear event -> self -> enemy -> will delete? *)
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

  (* Clears all bullets *)
  val clearAll : t -> unit

  (* Update event for clearing all projectiles *)
  val updateClear : t -> unit

  (* Constructor *)
  val create : cons -> t

  (* Spawns a single type of projectile *)
  val spawn : t -> Player.t -> bullet_type -> acceleration -> position -> unit

  (* Updates movement for projectiles *)
  val update : t -> unit

  (* Collision handling for all projectiles *)
  val collideAll : t -> unit

  (* Returns projectile data *)
  val getData : t -> bullet list
end

module Make : MakeType = functor (C : Collider) -> struct
  type t = {
    mutable bullets : bullet list;
    mutable clear : bool;
    red : Player.t;
    blue : Player.t
  }

  type cons = Player.t * Player.t(*  * Npc.t *)

  let clearAll (x : t) : unit = 
    List.iter (fun x -> add_update (DeleteBullet x.b_id)) x.bullets;
    x.bullets <- []

  (* Tells the projectiles to be cleared this step *)
  let setClear (x : t) () : unit = x.clear <- true

  (* Clears during time step if clear is set*)
  let updateClear (x : t) : unit =
    if x.clear then begin
      clearAll x;
      x.clear <- false
    end else ()

  (* Checks if two hitboxes are colliding *)
  let isCollide (p1, r1 : hitbox) (p2, r2 : hitbox) : bool =
    distance p1 p2 < r1 +. r2

  (* Gets the hitbox of a single projectile *)
  let getHitbox (b : bullet) : hitbox =
    (b.b_pos, float_of_int b.b_radius)

  (* Updates the position of a single projectile *)
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

  (* Checks the collision and grazing against a single player *)
  let checkPlayer (x : t) (b : bullet) (p : Player.t) (e : Player.t) : bool =
    if C.collide (Player.getColor p) b.b_color &&
      isCollide (getHitbox b) (Player.getHitbox p)
      then C.playerEvent (setClear x) p e
    else if C.graze (Player.getColor p) b.b_color &&
      isCollide (getHitbox b) (Player.getGrazebox p)
      then C.grazeEvent p
    else false

  (* Handles a single collision *)
  let collideOne (x : t) red blue (a : bullet list) (b : bullet) : bullet list =
    if checkPlayer x b red blue || checkPlayer x b blue red
    then (add_update (DeleteBullet b.b_id); a) else b::a

  (* Adds a bullet to the gui and the bullet list *)
  let addBullet (a : bullet list) (b : bullet) : bullet list =
    add_update (AddBullet (b.b_id, b.b_color, b.b_type, b.b_pos));
    b::a
  
  (* Instantiates a projectile handler *)
  let create (red, blue : cons) : t = { bullets = [];
                                        clear = false;
                                        red = red;
                                        blue = blue }

  (* Spawns a single type of projectile *)
  let spawn (x : t) player b_type accel pos : unit =
    if Player.reduceCharge player (cost_of_bullet b_type) then begin
      let new_b : bullet list = C.spawn player b_type accel pos in
      x.bullets <- (List.fold_left addBullet x.bullets new_b)
    end else ()

  (* Updates movement for all projectiles *)
  let update (x : t) : unit =
    x.bullets <- (List.fold_left updatePos [] x.bullets)
  
  (* Collision handling for all projectiles *)
  let collideAll (x : t) : unit =
    x.bullets <- (List.fold_left (collideOne x x.red x.blue) [] x.bullets)

  (* Returns projectile data *)
  let getData (x : t) : bullet list = x.bullets
end