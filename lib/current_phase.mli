open! Core

type t =
  | Enter_user
  | Waiting
  | Playing
  | End
[@@deriving sexp, bin_io, equal]
