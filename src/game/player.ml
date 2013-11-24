open Definitions
open Constants
open Util
open Netgraphics

(* lives, bombs, score, power, charge, player, move list, focus *)
type t = team_data ref * ((direction * direction) list ref) * bool ref
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

let setMoves (x : t) (lst : (direction * direction) list) : unit =
  match x with (t, m, f) -> m := lst

let setFocus (x : t) (b : bool) : unit =
  match x with (t, m, f) -> f := b

let create (c : color) : t =
  let id : id = next_available_id () in
  let pos : position = get_start_pos c in
  let p : player_char = { p_id = id;
                          p_pos = pos;
                          p_focused = false;
                          p_radius = cHITBOX_RADIUS;
                          p_color = c } in
  initGui id c pos;
  (ref (cINITIAL_LIVES, cINITIAL_BOMBS, 0, 0, 0, p), ref [], ref false)

let getSpeed (f : bool) : float =
  if f then (float_of_int cFOCUSED_SPEED) else (float_of_int cUNFOCUSED_SPEED)

let moveOff (pos : position) (off : vector) : position =
  let new_pos  = add_v pos off in
  if in_bounds new_pos then new_pos else pos

let update (x : t) : unit =
  match x with (t, m, f) ->
  match !t with (l, b, s, p, c, player) ->
  (* Calculates the amount the player needs to move by *)
  let off : float * float = match !m with
    | []   -> (0., 0.)
    | h::t -> vector_of_dirs h (getSpeed !f) in
  let pos : position = moveOff player.p_pos off in
  let new_player : player_char = { p_id = player.p_id;
                                   p_pos = pos;
                                   p_focused = !f;
                                   p_radius = player.p_radius;
                                   p_color = player.p_color } in
  add_update (MovePlayer (player.p_id, pos));
  t := (l, b, s, p, c, new_player)

let getData (x : t) : team_data =
  match x with (t, m, f) -> !t