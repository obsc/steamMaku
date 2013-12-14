open Team
open Definitions
open Constants
open Util

(*------------------------------Handling opponents-------------------------------*)
let enemy_loc : position ref = ref (0.0, 0.0)
let self_loc : position ref = ref (0.0, 0.0)
let self : color ref = ref Red
let bombs : int ref = ref 0
let lives : int ref = ref cINITIAL_LIVES
let invincible : int ref = ref 0

let set_self_and_enemy (e: position) (s: position) =
  self_loc := s;
  enemy_loc := subt_v e s

let handle_players (t1 : team_data) (t2 : team_data) : unit = 
  match t1,t2 with
  | (l1,b1,s1,p1,c1,player1),(l2,b2,s2,p2,c2,player2) ->
      if !self = player1.p_color then
        begin
        bombs := b1;
        if l1 < !lives then 
          begin
            lives := l1;
            invincible := cINVINCIBLE_FRAMES
          end;
        set_self_and_enemy player2.p_pos player1.p_pos
        end
      else 
        begin
        bombs := b2;
        if l2 < !lives then 
          begin
            lives := l2;
            invincible := cINVINCIBLE_FRAMES
          end;
        set_self_and_enemy player1.p_pos player2.p_pos
        end

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

(*---------------------------------Handling Movement---------------------------------*)
(* We want to apply pressure if we're invincible, otherwise stay close to the middle of the grid *)
let next_dir : (direction * direction) ref = ref (Neutral, Neutral)

let enemy_direction () : (direction * direction)= 
  match !enemy_loc with 
  | (x,y) -> 
    if x > 100. then 
      begin
        if y > 100. then (South, East)
        else if y < -100. then (North, East)
        else (East, Neutral)
      end
    else if x < -100. then
      begin
        if y > 100. then (South, West)
        else if y < -100. then (North, West)
        else (West, Neutral)
      end
    else 
      begin
        if y > 100. then (South, Neutral)
        else if y < -100. then (North, Neutral)
        else (Neutral, Neutral)
      end

let center_direction () : (direction * direction) = 
  match subt_v (float_of_int (cBOARD_WIDTH / 2), float_of_int (cBOARD_HEIGHT / 2)) !self_loc with
  | (x,y) -> 
    if x > 25. then 
      begin
        if y > 25. then (South, East)
        else if y < -25. then (North, East)
        else (East, Neutral)
      end
    else if x < -25. then
      begin
        if y > 25. then (South, West)
        else if y < -25. then (North, West)
        else (West, Neutral)
      end
    else 
      begin
        if y > 25. then (South, Neutral)
        else if y < -25. then (North, Neutral)
        else (Neutral, Neutral)
      end

let print_direction (move: direction * direction) : unit = 
  match move with 
  | (North, East) -> print_endline "northeast"
  | (North, West) -> print_endline "northwest"
  | (North, Neutral) -> print_endline "north"
  | (East, Neutral) -> print_endline "east"
  | (West, Neutral) -> print_endline "west"
  | (South, East) -> print_endline "southeast"
  | (South, West) -> print_endline "southwest"
  | (South, Neutral) -> print_endline "South"
  | (_, _) -> print_endline "Neutral"

let can_dodge (move : direction * direction) (bullets : bullet list) : bool =
  let can_dodge_helper pos =
    let wont_collide acc next =
      if next.b_color != !self then
        let x_factor = fst (subt_v pos next.b_pos) /. (fst next.b_vel) in
        let y_factor = snd (subt_v pos next.b_pos) /. (snd next.b_vel) in
        let proj_loc_x = add_v next.b_pos (scale x_factor next.b_vel) in
        let proj_loc_y = add_v next.b_pos (scale y_factor next.b_vel) in
        let no_collide_x = (distance !self_loc proj_loc_x > float_of_int (cHITBOX_RADIUS + next.b_radius)) in
        let no_collide_y = (distance !self_loc proj_loc_y > float_of_int (cHITBOX_RADIUS + next.b_radius)) in
        acc && (no_collide_x && no_collide_y)
      else acc in
    List.fold_left (wont_collide) true bullets in
  let pos = vector_of_dirs move (float_of_int cFOCUSED_SPEED) in
  can_dodge_helper (add_v pos !self_loc)

let dodge_direction (bullets : bullet list) : unit = 
  let move_list = [(South, Neutral); (South, West); (South, East);
                   (North, Neutral); (North, West); (North, East); 
                   (West, Neutral); (East, Neutral); center_direction ()] in
  let dodge_helper acc next =
    if can_dodge next bullets then next
    else acc in
  next_dir := List.fold_left dodge_helper (Neutral, Neutral) move_list;
  print_direction !next_dir

(*----------------------------------Handling Bombs----------------------------------*)
let use_bomb : bool ref = ref false

let handle_bombs (bullets : bullet list) : unit = 
  let will_collide acc next =
    if next.b_color != !self then
      let next_loc = add_v next.b_pos next.b_vel in
      acc || (distance !self_loc next_loc < float_of_int (cHITBOX_RADIUS + next.b_radius))
    else acc in
  use_bomb := List.fold_left (will_collide) false bullets

(* [receive_data d] is called whenever a game update comes from the server.
 * It's up to you what to do with this update. *)
let receive_data (d : game_data) : unit =
  match d with 
  | (t1, t2, ufos, bullets, powers) -> 
      handle_players t1 t2;
      handle_ufos ufos;
      handle_bombs bullets;
      dodge_direction bullets

let () = Random.self_init ()

let count = ref 0

let bot c =
  self := c;
  while true do
    (* Movement *)
    let dir = 
      if !invincible > 0 then 
        begin 
          send_action (Focus false);
          invincible := !invincible - 1;
          enemy_direction () 
        end
      else 
        begin
          send_action (Focus true);
          !next_dir
        end in
    let () = send_action (Move [dir]) in
    let () = if !use_bomb then
      begin 
        send_action (Bomb);
        if !bombs > 0 then 
          begin
            invincible := cINVINCIBLE_FRAMES
          end;
        use_bomb := false
      end in
    incr count;
    Thread.delay 0.05
  done

let () = start_bot bot receive_data
