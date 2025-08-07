open! Core

type t =
  | Entering_name of string * string option
  | Accepted_name of string
  [@@deriving sexp]

let name t =
  match t with Entering_name (name, _) -> name | Accepted_name name -> name
;;
