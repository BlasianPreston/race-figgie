type order_type =
  | Bid
  | Ask
[@@deriving equal, compare, hash, sexp]

type t =
  { player_id : string
  ; racer : Racer.t
  ; price : int option
  ; order_type : order_type
  }

val create
  :  player_id:string
  -> racer:Racer.t
  -> price:int option
  -> order_type:order_type
  -> t

val is_no_order : t -> bool
