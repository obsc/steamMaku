open Definitions
open Constants
open Util
open Netgraphics

type game = {
  mutable t : int;
  red : Player.t;
  blue : Player.t;
  bullets : Bullet.t;
  ufos : Npc.t;
  powers : Powerup.t
}

let init_game () : game =
  let red = Player.create Red in
  let blue = Player.create Blue in
  let ufos = Npc.create (red, blue) in
  { t = 0;
    red = red;
    blue = blue;
    bullets = Bullet.create (red, blue, ufos);
    ufos = ufos;
    powers = Powerup.create (red, blue, ufos) }

let time_up game : bool =
  (float_of_int game.t) *. cUPDATE_TIME >= cTIME_LIMIT

let get_result game : result =
  let rLives : int = Player.getLives game.red in
  let bLives : int = Player.getLives game.blue in
  let rScore : int = Player.getScore game.red in
  let bScore : int = Player.getScore game.blue in
  if time_up game || rLives = 0 || bLives = 0
    then begin
      if rLives > bLives then Winner Red
      else if rLives < bLives then Winner Blue
      else if rScore > bScore then Winner Red
      else if rScore < bScore then Winner Blue
      else Tie
    end
  else Unfinished

let spawnUfo game : unit =
  if game.t mod cUFO_SPAWN_INTERVAL = 0 
  then Npc.spawn game.ufos
  else ()

let handle_time game =
  game.t <- game.t + 1;
  spawnUfo game;
  Bullet.update game.bullets;
  Powerup.update game.powers;
  Npc.update game.ufos;
  Player.update game.red;
  Player.update game.blue;
  Bullet.collideAll game.bullets;
  Player.updateCharge game.red;
  Player.updateCharge game.blue;
  Bullet.updateClear game.bullets;
  (game, get_result game)

let shootBullet game p b_type pos acc : unit =
  if Player.reduceCharge p (cost_of_bullet b_type) then
  Bullet.spawn game.bullets (Player.getPos p) (Player.getColor p) b_type acc pos
  else ()

let handle_action game col act =
  let p : Player.t = match col with
    | Red  -> game.red
    | Blue -> game.blue in
  begin match act with
    | Move lst -> Player.setMoves p lst
    | Shoot (b_type, pos, acc) -> shootBullet game p b_type pos acc
    | Focus b -> Player.setFocus p b
    | Bomb -> if Player.bomb p then Bullet.clearAll game.bullets else ()
  end;
  game

let get_data game =
  (Player.getData game.red, Player.getData game.blue,
    Npc.getData game.ufos, Bullet.getData game.bullets, [])

