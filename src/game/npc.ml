open Definitions
open Constants
open Util
open Netgraphics

type npctype = Simple
type behavior = ufo ref -> unit
type t = (ufo ref * behavior) list ref
type cons = unit

let () = Random.self_init ()

let f_width : float = float_of_int cBOARD_WIDTH
let f_height : float = float_of_int cBOARD_HEIGHT
let f_speed : float = float_of_int cUFO_SPEED

(* xpos and ypos are the functions that determine an npc's location*)
let get_start_pos (n : npctype) : position =
  match n with
  | Simple -> let top_or_bot : int = Random.int 2 in
    let x : float = (Random.float (0.5 *. f_width)) +. 0.25 *. f_width in
    let y : float = if top_or_bot = 0 then 0. else f_height in
    (x, y)

let get_behavior (n : npctype) : (behavior * position) = 
  match n with
  | Simple -> 
    (* Generate first direction *)
    let a = (Random.float f_width, Random.float f_height) in
    let simple (u : ufo ref) : unit = 
      let dest = ref a in
      (* update ufo position *)
      let n_pos = add_v (!u).u_pos !dest in
      (* update position *)
      dest := (Random.float f_width, Random.float f_height);
      (* update ufo velocity *) 
      u := {
        u_id = (!u).u_id;
        u_pos = n_pos;
        u_vel = scale f_speed !dest;
        u_radius = cUFO_RADIUS;
        u_red_hits = (!u).u_red_hits;
        u_blue_hits = (!u).u_blue_hits; 
      } in
    (simple, a)

let create (c : unit) : t = ref []

let spawn (n : npctype) (x : t) : unit = 
  let id : id = next_available_id () in
  match n with
  | Simple -> let pos : position = get_start_pos n in
              let bd = get_behavior n in
              let b : behavior = fst bd in
              let dest : position = snd bd in
              let u : ufo = {
                u_id = id;
                u_pos = pos;
                u_vel = scale f_speed dest;
                u_radius = cUFO_RADIUS;
                u_red_hits = 0;
                u_blue_hits = 0 } in
              x := (ref u, b)::(!x)

let update (x : t) : unit = 
  let update_one acc (u, b) = b u in
  List.fold_left (update_one) () !x