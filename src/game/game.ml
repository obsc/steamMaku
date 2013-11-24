open Definitions
open Constants
open Util
open Netgraphics

type ticks = int ref
type game = {
  t : ticks;
  red : Player.t;
  blue : Player.t;
  bullets : Bullet.t
}

let init_game () : game =
  let red = Player.create Red in
  let blue = Player.create Blue in
  { t = ref 0;
    red = red;
    blue = blue;
    bullets = Bullet.create (red, blue) }

let get_result game =
  let rScore : int = Player.getScore game.red in
  let bScore : int = Player.getScore game.blue in
  if rScore > bScore then Winner Red
  else if rScore = bScore then Tie
  else Winner Blue

let handle_time game =
  if (float_of_int !(game.t)) *. cUPDATE_TIME >= cTIME_LIMIT then begin
    let res : result = get_result game in
    add_update (GameOver res);
    (game, get_result game)
  end
  else begin
    incr game.t;
    Bullet.update game.bullets;
    Player.update game.red;
    Player.update game.blue;
    Player.updateCharge game.red;
    Player.updateCharge game.blue;
    (game, Unfinished)
  end

let handle_action game col act =
  let p : Player.t = match col with
    | Red  -> game.red
    | Blue -> game.blue in
  begin match act with
    | Move lst -> Player.setMoves p lst
    | Shoot (b_type, pos, acc) -> Bullet.spawn game.bullets p b_type acc pos
    | Focus b -> Player.setFocus p b
    | Bomb -> () end;
  game

let get_data game =
  (Player.getData game.red, Player.getData game.blue,
    [], Bullet.getData game.bullets, [])

