open Definitions

type radius = float
type hitbox = position * radius

module type Object = sig
  type t
  type cons

  val create : cons -> t
  val update : t -> unit
end

module type NpcType = sig
  include Object with type cons = unit
  
  val getData : t -> ufo
end