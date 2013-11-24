open Definitions
open Constants
open Util
open Projectile

module type BulletType = sig
  include Collider with type out = bullet list
end

module Bullet : BulletType = struct
  type t = int
  type out = bullet list

  let collideRed (c : color) : bool = c = Blue
  let collideBlue (c : color) : bool = c = Red

  let pEvent (c : color) (p : Player.t) : unit = failwith "hi"

  let toData (x : t) : bullet list = []
end

include Make (Bullet)