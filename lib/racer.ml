open Core

type t =
  | Red
  | Yellow
  | Blue
  | Green
[@@deriving equal, compare, hash, sexp, bin_io]

let to_string = function
  | Red -> "Red"
  | Blue -> "Blue"
  | Green -> "Green"
  | Yellow -> "Yellow"
;;

let of_string = function
  | "Red" -> Red
  | "Blue" -> Blue
  | "Green" -> Green
  | "Yellow" -> Yellow
  | _ -> Red
;;

let to_img = function
  | Red -> "../images/pink_character.png"
  | Blue -> "../images/updated_blue_character.png"
  | Green -> "../images/green_character.png"
  | Yellow -> "../images/updated_yellow_character.png"
;;

let compare x y = String.compare (to_string x) (to_string y)
let sexp_of_t t = String.sexp_of_t (to_string t)
let t_of_sexp sexp = of_string (String.t_of_sexp sexp)

include Comparable.Make (struct
    type nonrec t = t

    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)
