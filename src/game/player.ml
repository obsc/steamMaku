open Definitions
open Constants
open Util
open Netgraphics

type t = team_data ref * ((direction * direction) list ref)
type cons = color

let get_start_pos (c : color) : position =
  let m : float = match c with
    | Red  -> 0.125
    | Blue -> 0.875 in
  let x : float = m *. (float_of_int cBOARD_WIDTH) in
  let y : float = 0.5 *. (float_of_int cBOARD_HEIGHT) in
  (x, y)

let initGui (id : id) (c : color) (pos : position) : unit =
  add_update (AddPlayer (id, c, pos));
  add_update (SetLives (c, cINITIAL_LIVES));
  add_update (SetBombs (c, cINITIAL_BOMBS));
  add_update (SetScore (c, 0));
  add_update (SetPower (c, 0));
  add_update (SetCharge (c, 0))

let create (c : color) : t =
  let id : id = next_available_id () in
  let pos : position = get_start_pos c in
  let p : player_char = { p_id = id;
                          p_pos = pos;
                          p_focused = false;
                          p_radius = cHITBOX_RADIUS;
                          p_color = c } in
  initGui id c pos;
  (ref (cINITIAL_LIVES, cINITIAL_BOMBS, 0, 0, 0, p), ref [])

let update (x : t) : unit = ()

let getData (x : t) : team_data = match x with (t, _) -> !t