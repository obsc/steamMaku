open Definitions

module type Object = sig
  type t
  type cons

  val create : cons -> t
  val update : t -> unit
end

module type BulletType = sig
  include Object with type cons = unit
  
  val getBullet : t -> bullet
end

module type PowerType = sig
  include Object with type cons = unit
  
  val getPower : t -> power
end

module type NpcType = sig
  include Object with type cons = unit
  
  val getNpc : t -> ufo
end