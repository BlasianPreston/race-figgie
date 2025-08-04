open Core

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

let create ~player_id ~racer ~(price : int option) ~order_type =
  { player_id; racer; price; order_type }
;;

let is_bid t = match t.order_type with Bid -> true | Ask -> false
let is_no_order t = match t.price with None -> true | Some _ -> false
