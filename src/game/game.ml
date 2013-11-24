open Definitions
open Constants
open Util

type ticks = int ref
type game = {
  t : ticks;
  red : Player.t;
  blue : Player.t
}

let init_game () : game = { t = ref 0;
                            red = Player.create Red;
                            blue = Player.create Blue }

let handle_time game =
  incr game.t;
  Player.update game.red;
  Player.update game.blue;
  (game, Unfinished)

let handle_action game col act = game

let get_data game =
  (Player.getData game.red, Player.getData game.blue, [], [], [])

