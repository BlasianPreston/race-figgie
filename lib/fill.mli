open! Core

type t =
  { buyer : string
  ; seller : string
  ; racer : Racer.t
  ; price : int
  }
[@@deriving sexp, bin_io, equal]

val create : string -> string -> Racer.t -> int -> t
