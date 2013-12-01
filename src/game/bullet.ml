open Definitions
open Constants
open Util
open Projectile
open Player

module Bullet : Collider = struct
  (* Check for collisions and grazing for opposite player and bullet colors *)
  let collide (p_col : color) (b_col : color) : bool = p_col <> b_col
  let graze (p_col : color) (b_col : color) : bool = p_col <> b_col

  (* Enemy's bullet has hit p *)
  let playerEvent (clear : unit -> unit) p enemy : bool =
    (* Upon player death, clear bullets, add score to other player *)
    let event () : unit =
      clear ();
      Player.killedOther enemy in
    Player.hit event p

  (* Player grazes *)
  let grazeEvent (p : Player.t) : bool = Player.graze p

  (* Spawns a type of bullet *)
  let spawn p b_type a target : bullet list =
    let speed = float_of_int (speed_of_bullet b_type) in
    let radius = radius_of_bullet b_type in
    let pos = getPos p in
    let c = getColor p in
    let tar = unit_v (subt_v target pos) in
    match b_type with
    | Bubble -> let b : bullet = {
                  b_type = Bubble;
                  b_id = next_available_id ();  
                  b_pos = pos;
                  b_vel = scale speed tar;
                  b_accel = a;
                  b_radius = radius;
                  b_color = c } in
                [b]

    | Spread -> 
      let angle_increment = rad_to_deg (360. /. float_of_int cSPREAD_NUM) in
      let rec num_to_spread acc i angle = 
        if i = 0 then acc
        else 
          let b : bullet = {
            b_type = Spread;
            b_id = next_available_id ();  
            b_pos = pos;
            b_vel = scale (float_of_int cSPREAD_SPEED) angle;
            b_accel = a;
            b_radius = radius;
            b_color = c } in
          num_to_spread (b::acc) (i-1) (rotate angle angle_increment) in
        num_to_spread [] cSPREAD_NUM tar

    | Trail  -> 
      let rec trail_three speed acc i = 
        if i = -1 then acc
        else 
          let angle = if i = 0 then 0.
                      else if i = 1 then rad_to_deg (float_of_int cTRAIL_ANGLE)
                      else rad_to_deg (float_of_int (~- cTRAIL_ANGLE)) in
          let b : bullet = {
            b_type = Trail;
            b_id = next_available_id ();  
            b_pos = pos;
            b_vel = scale (float_of_int speed) (rotate tar angle);
            b_accel = a;
            b_radius = radius;
            b_color = c } in
          trail_three speed (b::acc) (i-1) in
      let rec num_to_trail acc i = 
        if i = 0 then acc
        else 
          let speed_trail = trail_three (cTRAIL_SPEED_STEP * i) [] 2 in
          num_to_trail (speed_trail@acc) (i-1) in
      num_to_trail [] cTRAIL_NUM 
    | Power  -> []
end

include Make (Bullet)