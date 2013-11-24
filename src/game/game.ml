open Definitions
open Constants
open Util

(* TODO: change this *)
type ticks = int ref
type game = ticks

let init_game () : game = ref 1

let handle_time game = (game, Unfinished)

let handle_action game col act = game

let get_data game =
  failwith "I'm the strongest!"

