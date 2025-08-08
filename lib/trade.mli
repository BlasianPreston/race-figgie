open! Core

val get_best_bid_order : bid_order_list:Order.t list -> (int * string) option
val get_best_ask_order : ask_order_list:Order.t list -> (int * string) option
val match_orders : Game_state.t -> Game_state.t

val get_best_bids_and_asks
  :  Game_state.t
  -> Racer.t * (string * string) list
