open Definitions
open Constants

type radius = float
type hitbox = position * radius

let f_width : float = float_of_int cBOARD_WIDTH
let f_height : float = float_of_int cBOARD_HEIGHT

(* Abstract object with constructor and updater *)
module type Object = sig
  type t
  type cons

  val create : cons -> t
  val update : t -> unit
end