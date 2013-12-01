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
  id : int;
  mutable pos : position;
  mutable focused : bool;
  radius : int;
  color : color;
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

(* Setters for automatically sending gui updates *)
let setBombs (x : t) (b : int) : unit =
  x.bombs <- b;
  add_update (SetBombs (x.color, x.bombs))

let setPower (x : t) (p : int) : unit =
  x.power <- p;
  add_update (SetPower (x.color, x.power))

let addScore (x : t) (s : int) : unit =
  x.score <- x.score + s;
  add_update (SetScore (x.color, x.score))

let addCharge (x : t) (c : int) : unit =
  let new_charge = min (x.charge + c) cCHARGE_MAX in
  x.charge <- new_charge;
  add_update (SetCharge (x.color, x.charge))

let addLives (x : t) (l : int) : unit =
  x.lives <- x.lives + l;
  add_update (SetLives (x.color, x.lives))

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
  let newPos = add_v pos offset in
  if in_bounds newPos then newPos else pos

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
  addCharge x (cCHARGE_RATE + x.power)

(* Uses up cost amount of charge to shoot a bullet
 * Returns true if successful *)
let reduceCharge (x : t) (cost : int) : bool =
  if x.charge >= cost then begin
    addCharge x (-cost); true
  end else false

(* Player is hit by an enemy bullet 
 * Returns true to destroy colliding bullet *)
let hit (event : unit -> unit) (x : t) : bool =
  match x.status with
    | Normal -> begin
      addLives x (-1);
      setBombs x cINITIAL_BOMBS;
      setPower x (x.power / 2);
      x.status <- Mercy cINVINCIBLE_FRAMES;
      event ();
      true
    end
    | _      -> true

(* Player is grazing an enemy bullet 
 * Returns false to not destroy bullet *)
let graze (x : t) : bool =
  addScore x cGRAZE_POINTS;
  add_update (Graze);
  false

(* Has killed other player: increases score *)
let killedOther (x : t) : unit =
  addScore x cKILL_POINTS

(* Getters for player state *)
let getHitbox (x : t) : hitbox = (x.pos, float_of_int x.radius)

let getGrazebox (x : t) : hitbox = (x.pos, float_of_int cGRAZE_RADIUS)

let getPos (x : t) : position = x.pos

let getColor (x : t) : color = x.color

let getScore (x : t) : int = x.score

let getLives (x : t) : int = x.lives

(* Returns team data of the player *)
let getData (x : t) : team_data =
  let player : player_char = { p_id = x.id;
                               p_pos = x.pos;
                               p_focused = x.focused;
                               p_radius = x.radius;
                               p_color = x.color } in
  (x.lives, x.bombs, x.score, x.power, x.charge, player)