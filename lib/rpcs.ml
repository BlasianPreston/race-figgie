open! Core
open! Async_rpc_kernel

module Client_message = struct
  module Query = struct
    type t =
      | New_player of string
      | Everyone_ready
      | Order_placed of Order.t
      | Order_filled of Fill.t
    [@@deriving sexp_of, bin_io]
  end

  module Response = struct
    type t = (string, string) Result.t
    (* maybe change to custom*)
    [@@deriving sexp_of, bin_io]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"client-message"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Only_on_exn
  ;;
end

module Poll_client_state = struct
  module Query = struct
    type t = { name : string } [@@deriving sexp, bin_io, equal]
  end

  module Response = struct
    type t = Client_state.t [@@deriving sexp, bin_io, equal]

    module Update = struct
      type nonrec t = t [@@deriving sexp, bin_io, equal]
    end

    let diffs ~from ~to_ =
      ignore from;
      to_
    ;;

    let update t update =
      ignore t;
      update
    ;;
  end

  module Error = struct
    type t = string [@@deriving sexp, bin_io]
  end

  let rpc =
    Polling_state_rpc.create
      ~name:"poll-client-state"
      ~version:0
      ~query_equal:[%equal: Query.t]
      ~bin_query:Query.bin_t
      (module Response)
  ;;
end
