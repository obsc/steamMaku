open Definitions

type radius = float
type hitbox = position * radius

module type Object = sig
  type t
  type cons

  val create : cons -> t
  val update : t -> unit
end