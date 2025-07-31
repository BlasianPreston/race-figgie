open! Core

(** A map keyed by [Racer.t] values. *)
module Racer : sig
  type t =
    | Red
    | Yellow
    | Blue
    | Green
  [@@deriving equal, compare, hash, sexp_of, sexp]

  val to_string : t -> string
  val of_string : string -> t
  val to_img : t -> string
  val sexp_of_t : t -> Base.Sexp.t
  val t_of_sexp : Base.Sexp.t -> t

  include Core.Comparable.S with type t := t
end

type position = int (* lap position or index *)
type velocity = int

type order_type =
  | Bid
  | Ask
  [@@deriving equal, compare, hash, sexp]

module Order : sig
  type t =
    { player_id : string
    ; racer : Racer.t
    ; price : int option
    ; order_type : order_type
    } [@@deriving equal, compare, hash, sexp]

  val create
    :  player_id:string
    -> racer:Racer.t
    -> price:int option
    -> order_type:order_type
    -> t

  val is_no_order : t -> bool
end

module Fill : sig
  type t =
    { buyer : string
    ; seller : string
    ; racer : Racer.t
    ; price : int
    }

  val create : string -> string -> Racer.t -> int -> t
end

type trade =
  { buyer : string
  ; seller : string
  ; racer : Racer.t
  ; price : int
  }

module Player : sig
  type t =
    { id : string
    ; holdings : Racer.t list
    ; cash : int
    }
  [@@deriving compare, hash, sexp_of]

  val create : string -> t
  val create_with_holdings : string -> Racer.t list -> t
end

module State : sig
  type t =
    { players : Player.t String.Map.t
    ; bids : Order.t list Racer.Map.t
    ; asks : Order.t list Racer.Map.t
    ; filled_orders : Fill.t list
    ; race_positions : (Racer.t * position * velocity) list
    ; winner : Racer.t option
    }

  val create
    :  players:Player.t String.Map.t
    -> bids:Order.t list Racer.Map.t
    -> asks:Order.t list Racer.Map.t
    -> filled_orders:Fill.t list
    -> race_positions:(Racer.t * velocity * velocity) list
    -> winner:Racer.t option
    -> t

  val update
    :  players:Player.t String.Map.t
    -> bids:Order.t list Racer.Map.t
    -> asks:Order.t list Racer.Map.t
    -> filled_orders:Fill.t list
    -> race_positions:(Racer.t * velocity * velocity) list
    -> winner:Racer.t option
    -> t

  val update_positions : t -> t
  val update_velocities : t -> t
  val set_winner : t -> Racer.t option -> t
  val empty : unit -> t
end
