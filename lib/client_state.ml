open Core

(* add representation of previous results for *)
type t =
  { current_phase : Current_phase.t
  ; all_trades : Fill.t list
  ; players : Player.t list
  ; ready_players : Player.t list
  ; shown_racers : Racer.t list
  ; me : Player.t
  }
[@@deriving sexp, bin_io, equal]
