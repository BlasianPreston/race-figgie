open! Core

type position = int (* lap position or index *)
type velocity = int

type t =
  { current_phase : Current_phase.t
  ; players : Player.t String.Map.t
  ; bids : Order.t list Racer.Map.t
  ; asks : Order.t list Racer.Map.t
  ; filled_orders : Fill.t list
  ; race_positions : (Racer.t * position * velocity) list
  ; winner : Racer.t option
  }

val create
  :  current_phase:Current_phase.t
  -> players:Player.t String.Map.t
  -> bids:Order.t list Racer.Map.t
  -> asks:Order.t list Racer.Map.t
  -> filled_orders:Fill.t list
  -> race_positions:(Racer.t * velocity * velocity) list
  -> winner:Racer.t option
  -> t

val update
  :  current_phase:Current_phase.t
  -> players:Player.t String.Map.t
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
val get_client_state_from_name : t -> string -> Client_state.t
val name_taken : t -> string -> bool
val add_player : t -> string -> t
val initialize_racers : t -> t
val add_player_and_possibly_add_hand : t -> string -> t
val add_order : t -> Order.t -> t
val add_fill : t -> Fill.t -> t
val take_money_for_pot : t -> t
val reset_hands : t -> t
val add_hands_to_players : t -> t
