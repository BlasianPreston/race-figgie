open! Core

type t =
  | Waiting
  | Playing
  | End
[@@deriving sexp, bin_io, equal]