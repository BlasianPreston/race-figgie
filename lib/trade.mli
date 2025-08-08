open! Core

val get_best_bid_order : bid_order_list:Order.t list -> (int * string) option
val get_best_ask_order : ask_order_list:Order.t list -> (int * string) option


