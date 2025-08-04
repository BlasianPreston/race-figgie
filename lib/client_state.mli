open Core

type t =
  { current_phase : Current_phase.t
  ; all_trades : Fill.t list
  ; players : Player.t list
  ; ready_players : Player.t list
  ; shown_racers : Racer.t list
  ; my_red_bid : int option
  ; my_yellow_bid : int option
  ; my_blue_bid : int option
  ; my_green_bid : int option
  ; my_red_ask : int option
  ; my_yellow_ask : int option
  ; my_blue_ask : int option
  ; my_green_ask : int option
  ; me : Player.t
  }
[@@deriving sexp, bin_io, equal]