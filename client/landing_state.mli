open! Core

type t =
  | Entering_name of string * string option
  | Accepted_name of string
[@@deriving sexp]

val name : t -> string
