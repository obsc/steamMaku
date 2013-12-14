open Team
open Definitions
open Constants
open Util

(*------------------------------Handling opponents-------------------------------*)
let enemy_loc : position ref = ref (0.0, 0.0)
let self_loc : position ref = ref (0.0, 0.0)
let self : color ref = ref Red

let set_self_and_enemy (e: position) (s: position) =
  self_loc := s;
  enemy_loc := subt_v e s

let handle_players (t1 : team_data) (t2 : team_data) : unit = 
  match t1,t2 with
  | (l1,b1,s1,p1,c1,player1),(l2,b2,s2,p2,c2,player2) ->
      if !self = player1.p_color then
        set_self_and_enemy player2.p_pos player1.p_pos
      else set_self_and_enemy player1.p_pos player2.p_pos

let target_loc () = 
  let x = Random.float 600. in
  let y = Random.float 600. in
  add_v !enemy_loc (x -. 300., y -. 300.)
let target_accel () =
  let x = Random.float 60. in
  let y = Random.float 60. in
  let accel_target = unit_v (add_v !enemy_loc (x -. 30., y -. 30.)) in
  scale cACCEL_LIMIT accel_target

(*---------------------------------Handling UFOS---------------------------------*)
let ufo_loc : (int * position) option ref = ref None

let is_alive (id : int) (ufos : ufo list) = 
  let is_id acc next = 
    if next.u_id = id then true
    else acc in
  List.fold_left (is_id) false ufos

let set_ufo_target (ufos : ufo list) =
  if List.length ufos > 0 then
    begin 
      let first = List.hd ufos in
      ufo_loc := Some (first.u_id, first.u_pos)
    end
  else 
    ufo_loc := None

let handle_ufos (ufos : ufo list) : unit = 
  match !ufo_loc with
  | None -> set_ufo_target ufos
  | Some (id,_) -> if not (is_alive id ufos) then set_ufo_target ufos

(*----------------------------------Handling Bombs----------------------------------*)
let use_bomb : bool ref = ref false

let handle_bombs (bullets : bullet list) : unit = 
  let will_collide acc next =
    if next.b_color != !self then
      let next_loc = add_v next.b_pos next.b_vel in
      acc || (distance !self_loc next_loc < float_of_int (cHITBOX_RADIUS + next.b_radius))
    else acc in
  use_bomb := List.fold_left (will_collide) false bullets

(*---------------------------------Handling Movement---------------------------------*)
(* We want to apply pressure if we're invincible, otherwise stay close to the middle of the grid *)

(* [receive_data d] is called whenever a game update comes from the server.
 * It's up to you what to do with this update. *)
let receive_data (d : game_data) : unit =
  match d with 
  | (t1, t2, ufos, bullets, powers) -> 
      handle_players t1 t2;
      handle_ufos ufos;
      handle_bombs bullets

let () = Random.self_init ()

let count = ref 0

let rand_direction () = let roll = Random.int 8 in
  if roll = 0 then (North, Neutral)
  else if roll = 1 then (North, East)
  else if roll = 2 then (East, Neutral)
  else if roll = 3 then (East, South)
  else if roll = 4 then (South, Neutral)
  else if roll = 5 then (South, West)
  else if roll = 6 then (West, Neutral)
  else (West, North)

let bot c =
  self := c;
  while true do
    let d = rand_direction () in
    let () = send_action (Move [d;d;d;d]) in
    let () = if !use_bomb then
      begin 
        send_action (Bomb);
        use_bomb := false
      end in
    begin
      match !ufo_loc with 
      | None -> ()
      | Some (_, pos) -> send_action (Shoot (Trail, pos, scale cACCEL_LIMIT (unit_v pos)))
    end;
    incr count;
    Thread.delay 0.05
  done

let () = start_bot bot receive_data