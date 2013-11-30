open Definitions
open Constants
open Gameutil
open Util
open Netgraphics

type invulnerable = | Normal
                    | Mercy of int
                    | Bombing of int

type t = {
  mutable lives : int;
  mutable bombs : int;
  mutable score : int;
  mutable power : int;
  mutable charge : int;
  mutable moves : (direction * direction) list;
  mutable id : int;
  mutable pos : position;
  mutable focused : bool;
  mutable radius : int;
  mutable color : color;
  mutable status : invulnerable
}

type cons = color

(* Gets the speed based on its focus state *)
let getSpeed (f : bool) : float =
  if f then (float_of_int cFOCUSED_SPEED) else (float_of_int cUNFOCUSED_SPEED)

(* Gets the starting position of a player *)
let getStartPos (c : color) : position =
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
  x.moves <- lst

(* Called upon receiving a focus action *)
let setFocus (x : t) (b : bool) : unit =
  x.focused <- b

(* Instantiates a player *)
let create (c : color) : t =
  let id : id = next_available_id () in
  let pos : position = getStartPos c in
  initGui id c pos;
  { lives = cINITIAL_LIVES;
    bombs = cINITIAL_BOMBS;
    score = 0;
    power = 0;
    charge = 0;
    moves = [];
    id = id;
    pos = pos;
    focused = false;
    radius = cHITBOX_RADIUS;
    color = c;
    status = Normal }

(* Moves a position, bounding it to inside the field *)
let move (pos : position) (offset : vector) : position =
  let new_pos  = add_v pos offset in
  if in_bounds new_pos then new_pos else pos

(* A single update step: updates focus and position *)
let update (x : t) : unit =
  (* Calculates the amount the player needs to move by *)
  let off : float * float = match x.moves with
    | []   -> (0., 0.)
    | h::t -> vector_of_dirs h (getSpeed x.focused) in
  x.pos <- move x.pos off;
  add_update (MovePlayer (x.id, x.pos));
  (* Updates invulnerability timers *)
  match x.status with
    | Normal    -> ()
    | Mercy n   -> if n = 0 then x.status <- Normal
                   else x.status <- Mercy (n - 1)
    | Bombing n -> if n = 0 then x.status <- Normal
                   else x.status <- Bombing (n - 1)

(* Updates charge of the player *)
let updateCharge (x : t) : unit =
  x.charge <- min (x.charge + cCHARGE_RATE + x.power) cCHARGE_MAX;
  add_update (SetCharge (x.color, x.charge))

(* Uses up cost amount of charge to shoot a bullet
 * Returns true if successful *)
let reduceCharge (x : t) (cost : int) : bool =
  if x.charge >= cost then begin
    x.charge <- x.charge - cost; true
  end else false

(* Player is hit by an enemy bullet 
 * Returns true to destroy colliding bullet *)
let hit (event : unit -> unit) (x : t) : bool =
  match x.status with
    | Normal -> begin
      x.lives <- x.lives - 1;
      x.bombs <- cINITIAL_BOMBS;
      x.power <- x.power / 2;
      x.status <- Mercy cINVINCIBLE_FRAMES;
      add_update (SetLives (x.color, x.lives));
      add_update (SetBombs (x.color, x.bombs));
      add_update (SetPower (x.color, x.power));
      event ();
      true
    end
    | _      -> true

(* Player is grazing an enemy bullet 
 * Returns false to not destroy bullet *)
let graze (x : t) : bool =
  x.score <- x.score + cGRAZE_POINTS;
  add_update (SetScore (x.color, x.score));
  add_update (Graze);
  false

(* Has killed other player: increases score *)
let killedOther (x : t) : unit =
  x.score <- x.score + cKILL_POINTS;
  add_update (SetScore (x.color, x.score))

(* Getters for player state *)
let getHitbox (x : t) : hitbox = (x.pos, float_of_int x.radius)

let getGrazebox (x : t) : hitbox = (x.pos, float_of_int cGRAZE_RADIUS)

let getPos (x : t) : position = x.pos

let getColor (x : t) : color = x.color

let getScore (x : t) : int = x.score

let dead (x : t) : bool = x.lives <= 0

(* Returns team data of the player *)
let getData (x : t) : team_data =
  let player : player_char = { p_id = x.id;
                               p_pos = x.pos;
                               p_focused = x.focused;
                               p_radius = x.radius;
                               p_color = x.color } in
  (x.lives, x.bombs, x.score, x.power, x.charge, player)