open! Core
open! Async_rpc_kernel

module Client_message : sig
  module Query : sig
    type t =
      | New_player of string
      | Everyone_ready
      | Order_placed of Order.t
      | Order_filled of Fill.t
    [@@deriving sexp_of, bin_io]
  end

  module Response : sig
    type t = (string, string) Result.t
    (* maybe change to custom*) [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Rpc.Rpc.t
end

module Poll_client_state : sig
  module Query : sig
    type t = { name : string } [@@deriving sexp_of, bin_io, equal]
  end

  module Response : sig
    type t = Client_state.t [@@deriving sexp_of, bin_io, equal]

    module Update : sig
      type nonrec t = t [@@deriving bin_io, sexp_of, equal]
    end

    val diffs : from:t -> to_:t -> Update.t
    val update : t -> Update.t -> t
  end

  module Error : sig
    type t = string [@@deriving sexp_of, bin_io]
  end

  val rpc : (Query.t, Response.t) Polling_state_rpc.t
end
