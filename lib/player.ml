open! Core

type t =
  { id : string
  ; holdings : Racer.t list
  ; cash : int
  }
[@@deriving sexp, bin_io, equal]

let create name = { id = name; holdings = []; cash = 400 }

let create_with_holdings id (holdings : Racer.t list) =
  { id; holdings; cash = 400 }
;;
