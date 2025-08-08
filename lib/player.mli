open! Core

type t =
  { id : string
  ; holdings : Racer.t list
  ; cash : int
  }
[@@deriving sexp, bin_io, equal]

val create : string -> t
val create_with_holdings : string -> Racer.t list -> t
val richest_player_id : t list -> string