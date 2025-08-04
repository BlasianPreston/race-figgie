open! Core

type order_type =
  | Bid
  | Ask
[@@deriving equal, compare, hash, sexp, bin_io]

type t =
  { player_id : string
  ; racer : Racer.t
  ; price : int option
  ; order_type : order_type
  }
[@@deriving sexp, bin_io]

val create
  :  player_id:string
  -> racer:Racer.t
  -> price:int option
  -> order_type:order_type
  -> t

val is_no_order : t -> bool
val is_bid : t -> bool
