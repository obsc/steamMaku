open Definitions
open Constants
open Gameutil
open Util
open Netgraphics

type behavior = ufo -> int -> ufo
type time = int
type t = {
  mutable ufos : (ufo * behavior * time) list;
}
type cons = unit

let () = Random.self_init ()

let f_speed : float = float_of_int cUFO_SPEED

(* Initializes the list of npcs *)
let create (c : unit) : t = { ufos = [] }

(* xpos and ypos are the functions that determine an initial npc's location*)
let get_start_pos (n : npctype) : position =
  match n with
  | Simple -> let top_or_bot : int = Random.int 2 in
    let x : float = (Random.float (0.5 *. f_width)) +. 0.25 *. f_width in
    let y : float = if top_or_bot = 0 then 0. else f_height in
    (x, y)

(* Every NPC type has a unique behavior on a timestep 
 *    UFO types simply move in a random direction
 *)
let get_behavior (n : npctype) : behavior = 
  match n with
  | Simple -> 
    let simple (u : ufo) (time: int) : ufo = 
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
      { u with u_pos = n_pos; u_vel = n_vel} in
    simple

(* Spawns a single npc into the game based on its type *)
let spawn (n : npctype) (x : t) : unit = 
  let id : id = next_available_id () in
  match n with
  | Simple -> let pos : position = get_start_pos n in
              let b : behavior = get_behavior n in
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
  let update_one acc (u, b, t) = acc@[(b u t, b, t + 1)] in
  x.ufos <- List.fold_left (update_one) [] x.ufos

(* The hit UFO updates *)
let hit (x : t) (id : int) (shot_by : color) : bool =
  let hit_one acc (u, b, t) = 
    let n_ufo = 
      if u.u_id = id then 
        match shot_by with
        | Red -> { u with u_red_hits = u.u_red_hits + 1}
        | Blue -> { u with u_blue_hits = u.u_blue_hits + 1}
      else u in
    if n_ufo.u_red_hits + n_ufo.u_blue_hits = cUFO_HITS then add_update (DeleteUFO id); (* spawn powerups here *)
    if n_ufo.u_red_hits + n_ufo.u_blue_hits = cUFO_HITS then acc
    else acc@[(n_ufo, b, t)] in
  x.ufos <- List.fold_left (hit_one) [] x.ufos;
  true