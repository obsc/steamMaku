open Definitions
open Constants
open Util
open Projectile
open Player

module PowerUp : Collider = struct
  let collide (p_col : color) (b_col : color) : bool = true
  let graze (p_col : color) (b_col : color) : bool = false
  let collideNpc (b_col : color) : bool = false

  (* Player has run into p *)
  let playerEvent (clear : unit -> unit) p enemy : bool =
    Player.gainPower p

  let grazeEvent (p : Player.t) : bool = false
  let npcEvent (n : Npc.t) (c : color) (id : id) : bool = false

  (* Spawns a powerup *)
  let spawn p b_type a target : power =
    let tar = unit_v (subt_v target pos) in
    { b_type = Bubble;
      b_id = next_available_id ();  
      b_pos = pos;
      b_vel = scale (float_of_int (speed_of_bullet Power)) tar;
      b_accel = 0;
      b_radius = radius_of_bullet Power;
      b_color = Blue }
end

include Make (Bullet)