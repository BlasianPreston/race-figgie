type t =
    | Red
    | Yellow
    | Blue
    | Green
  [@@deriving equal, compare, hash, sexp_of, sexp, bin_io]

  val to_string : t -> string
  val of_string : string -> t
  val to_img : t -> string
  val sexp_of_t : t -> Base.Sexp.t
  val t_of_sexp : Base.Sexp.t -> t

  include Core.Comparable.S with type t := t