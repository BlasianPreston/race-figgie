open! Core

type t =
  { buyer : string
  ; seller : string
  ; racer : Racer.t
  ; price : int
  }
[@@deriving sexp, bin_io, equal]

let create buyer seller racer price = { buyer; seller; racer; price }
