open Definitions
open Constants
open Gameutil
open Util
open Netgraphics

(* lives, bombs, score, power, charge, player, move list, focus *)
type t = team_data ref * ((direction * direction) list ref) * bool ref
type cons = color

(* Gets the speed based on its focus state *)
let getSpeed (f : bool) : float =
  if f then (float_of_int cFOCUSED_SPEED) else (float_of_int cUNFOCUSED_SPEED)

(* Gets the starting position of a player *)
let get_start_pos (c : color) : position =
  let m : float = match c with
    | Red  -> 0.125
    | Blue -> 0.875 in
  let x : float = m *. (float_of_int cBOARD_WIDTH) in
  let y : float = 0.5 *. (float_of_int cBOARD_HEIGHT) in
  (x, y)

(* Initializes the gui for that player *)
let initGui (id : id) (c : color) (pos : position) : unit =
  add_update (AddPlayer (id, c, pos));
  add_update (SetLives (c, cINITIAL_LIVES));
  add_update (SetBombs (c, cINITIAL_BOMBS));
  add_update (SetScore (c, 0));
  add_update (SetPower (c, 0));
  add_update (SetCharge (c, 0))

(* Called upon receiving a move action *)
let setMoves (x : t) (lst : (direction * direction) list) : unit =
  match x with (t, m, f) -> m := lst

(* Called upon receiving a focus action *)
let setFocus (x : t) (b : bool) : unit =
  match x with (t, m, f) -> f := b

(* Instantiates a player *)
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

(* Moves a position, bounding it to inside the field *)
let moveOff (pos : position) (off : vector) : position =
  let new_pos  = add_v pos off in
  if in_bounds new_pos then new_pos else pos

(* A single update step: updates focus and position *)
let update (x : t) : unit =
  match x with (t, m, f) ->
  match !t with (l, b, s, p, c, player) ->
  (* Calculates the amount the player needs to move by *)
  let off : float * float = match !m with
    | []   -> (0., 0.)
    | h::t -> vector_of_dirs h (getSpeed !f) in
  let pos : position = moveOff player.p_pos off in
  let new_player : player_char = 
    { player with p_pos = pos; p_focused = !f } in
  add_update (MovePlayer (player.p_id, pos));
  t := (l, b, s, p, c, new_player)

(* Updates charge of the player *)
let updateCharge (x : t) : unit =
  match x with (t, m, f) ->
  match !t with (l, b, s, p, c, player) ->
  let charge : int = min (c + cCHARGE_RATE + p) cCHARGE_MAX in
  add_update (SetCharge (player.p_color, charge));
  t := (l, b, s, p, charge, player)

let getPlayer (x : t) : player_char =
  match x with (t, m, f) ->
  match !t with (l, b, s, p, c, player) ->
  player

let getHitbox (x : t) : hitbox =
  let p = getPlayer x in (p.p_pos, float_of_int p.p_radius)

let getPos (x : t) : position =
  let p = getPlayer x in p.p_pos

let getColor (x : t) : color =
  let p = getPlayer x in p.p_color

let getScore (x : t) : int =
  match x with (t, m, f) ->
  match !t with (l, b, s, p, c, player) ->
  s

(* Returns team data of the player *)
let getData (x : t) : team_data =
  match x with (t, m, f) -> !t