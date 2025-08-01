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


let create ~player_id ~racer ~(price : int option) ~order_type =
  { player_id; racer; price; order_type }
;;

let is_no_order t = match t.price with None -> true | Some _ -> false
