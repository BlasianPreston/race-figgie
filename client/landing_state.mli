type t =
  | Entering_name of string * string option
  | Accepted_name of string

val name : t -> string
