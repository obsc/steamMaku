open Definitions
open Constants
open Gameutil
open Util
open Netgraphics

type behavior = ufo -> int -> ufo
type time = int
type spawner = position -> color -> bullet_type
               -> acceleration -> position -> unit
type t = {
  mutable ufos : (ufo * behavior * time) list;
  red : Player.t;
  blue : Player.t;
  mutable spawn : spawner
}
type cons = Player.t * Player.t

let () = Random.self_init ()

let f_speed : float = float_of_int cUFO_SPEED

(* Initializes the list of npcs *)
let create (red, blue : cons) : t = { ufos = [];
                                      red = red;
                                      blue = blue;
                                      spawn = (fun a b c d e -> ()) }

let setSpawn (x : t) (spawner : spawner) : unit = 
  x.spawn <- spawner

(* xpos and ypos are the functions that determine an initial npc's location*)
let start_pos () : position =
  let top_or_bot : int = Random.int 2 in
  let x : float = (Random.float (0.5 *. f_width)) +. 0.25 *. f_width in
  let y : float = if top_or_bot = 0 then 0. else f_height in
  (x, y)

(* Every NPC type has a unique behavior on a timestep 
 *    UFO types simply move in a random direction
 *)
let behavior (u : ufo) (time: int) : ufo = 
  let n_vel = 
    (* new destination *)
    if time mod cUFO_MOVE_INTERVAL = 0 then
      unit_v (subt_v (Random.float f_width, Random.float f_height) u.u_pos)
    (* keep old destination *)
    else
      u.u_vel in
  (* update ufo position *)
  let n_pos = add_v u.u_pos n_vel in
  (* update ufo velocity *) 
  add_update (MoveUFO (u.u_id, n_pos));
  { u with u_pos = n_pos; u_vel = n_vel }

(* Spawns a single npc into the game based on its type *)
let spawn (x : t) : unit = 
  let id : id = next_available_id () in
  let pos : position = start_pos () in
  let b : behavior = behavior in
  let u : ufo = {
    u_id = id;
    u_pos = pos;
    u_vel = unit_v (subt_v (Random.float f_width, Random.float f_height) pos);
    u_radius = cUFO_RADIUS;
    u_red_hits = 0;
    u_blue_hits = 0 } in
  x.ufos <- (u, b, 1)::(x.ufos);
  add_update (AddUFO (id, pos))

(* Goes through every npc and performs an update *)
let update (x : t) : unit = 
  let update_one acc (u, b, t) = (b u t, b, t + 1)::acc in
  x.ufos <- List.fold_left (update_one) [] x.ufos

(* Returns a random location in the radius of the ufo *)
let getRandomPos (u : ufo) : position = 
  let s = Random.float (float_of_int cUFO_SCATTER_RADIUS) in
  let random = (Random.float f_width, Random.float f_height) in
  add_v (scale s (unit_v (subt_v random u.u_pos))) u.u_pos

(* Spawns powerups after the UFO dies *)
let rec spawnPowerups (x : t) (num_red : int) (num_blue : int) (u : ufo): unit =
  if num_red = 0 && num_blue = 0 then ()
  else if num_red > 0 then begin
    x.spawn (getRandomPos u) Red Power (0., 0.) (Player.getPos x.red);
    spawnPowerups x (num_red - 1) num_blue u
  end
  else begin 
    x.spawn (getRandomPos u) Blue Power (0., 0.) (Player.getPos x.blue);
    spawnPowerups x num_red (num_blue - 1) u
  end

(* The hit UFO updates *)
let hit (x : t) (id : int) (shot_by : color) : bool =
  let hit_one acc (u, b, t) = 
    let n_ufo = 
      if u.u_id = id then 
        match shot_by with
        | Red -> { u with u_red_hits = u.u_red_hits + 1}
        | Blue -> { u with u_blue_hits = u.u_blue_hits + 1}
      else u in
    if n_ufo.u_red_hits + n_ufo.u_blue_hits = cUFO_HITS then begin
      add_update (DeleteUFO id);
      let num_red = n_ufo.u_red_hits / cUFO_HITS * cUFO_POWER_NUM in
      let num_blue = n_ufo.u_blue_hits / cUFO_HITS * cUFO_POWER_NUM in
      spawnPowerups x num_red num_blue n_ufo
    end;
    if n_ufo.u_red_hits + n_ufo.u_blue_hits = cUFO_HITS then acc
    else (n_ufo, b, t)::acc in
  x.ufos <- List.fold_left (hit_one) [] x.ufos;
  true

(* The hitbox of all the npcs *)
let getHitbox (x : t) : (id * hitbox) list = 
  let to_hitbox (u, b, t) =
    (u.u_id, (u.u_pos, float_of_int cUFO_RADIUS)) in
  List.map to_hitbox x.ufos

let getData (x : t) : ufo list =
  List.map (fun (n_ufo, b, t) -> n_ufo) x.ufos