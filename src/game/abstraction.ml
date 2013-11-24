open Definitions
open Constants
open Util

module type Object = sig
  type t
  type cons

  val create : cons -> t
  val update : t -> unit
end

module type TeamType = sig
  include Object with type cons = color
  
  val getTeam : t -> team_data
end

module Team : TeamType = struct
  type t = team_data ref * (direction * direction) list
  type cons = color

  let get_start_loc (c : color) : position =
    failwith "todo"

  let create (c : cons) : t =
    let loc : position = get_start_loc c in
    let p : player_char = { p_id = next_available_id ();
                            p_pos = get_start_loc c;
                            p_focused = false;
                            p_radius = cHITBOX_RADIUS;
                            p_color = c } in
    (ref (cINITIAL_LIVES, cINITIAL_BOMBS, 0, 0, 0, p), ref [])
  let update (x : t) : unit = ()
  let getTeam (x : t) : team_data = failwith "hi"
end

module type BulletType = sig
  include Object with type cons = unit
  
  val getBullet : t -> bullet
end

module type PowerType = sig
  include Object with type cons = unit
  
  val getPower : t -> power
end

module type NpcType = sig
  include Object with type cons = unit
  
  val getNpc : t -> ufo
end