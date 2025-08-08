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
  ; race_positions : (Racer.t * int * int) list
  ; me : Player.t
  ; winner : Racer.t option
  ; pot_winner : string option
  ; best_orders : Racer.t * (string * string) list
  }
[@@deriving sexp, bin_io, equal]
