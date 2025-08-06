open! Core
open! Async_kernel
open! Async_rpc_kernel
open Bonsai_web
open Bonsai.Let_syntax
open! Race_figgie

let buying_power bp =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "buying_power" ] ]
    [ Vdom.Node.h2
        ~attrs:[ Vdom.Attr.classes [ "buying_power_text" ] ]
        [ Vdom.Node.text ("Buying Power: $" ^ Int.to_string bp) ]
    ]
;;

let body bp =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "race_column" ] ]
    [ Vdom.Node.h1
        ~attrs:[ Vdom.Attr.classes [ "race header" ] ]
        [ Vdom.Node.text "Race Figgie" ]
    ; Vdom.Node.img
        ~attrs:
          [ Vdom.Attr.classes [ "race_img" ]
          ; Vdom.Attr.src "../images/race.png"
          ]
        ()
    ; buying_power bp
    ]
;;

(* Dummy client state *)
let dcs : Client_state.t =
  { Client_state.current_phase = Current_phase.Playing
  ; all_trades =
      [ Fill.create "Preston" "Joseph" Racer.Blue 10
      ; Fill.create "Bari" "Fahim" Racer.Red 10
      ]
  ; players = []
  ; ready_players = []
  ; shown_racers = []
  ; my_red_bid = None
  ; my_yellow_bid = None
  ; my_blue_bid = None
  ; my_green_bid = Some 5
  ; my_red_ask = None
  ; my_yellow_ask = None
  ; my_blue_ask = None
  ; my_green_ask = None
  ; me = { id = "Joseph"; holdings = []; cash = 69 }
  }
;;

let page (local_ graph) =
  let dispatcher = Rpc_effect.Rpc.dispatcher Rpcs.Client_message.rpc graph in
  let helper_func =
    let%arr dispatcher in
    dispatcher
  in
  let default_client_state, _update_initial_state =
    Bonsai.state_machine_with_input
      ~default_model:dcs
      ~apply_action:(fun ctx input model action ->
        match input with
        | Bonsai.Computation_status.Inactive -> model
        | Active dispatcher ->
          (match action with
           | `Update_username username ->
             { dcs with me = { dcs.me with id = username } }
           | `Set_username ->
             let query = Rpcs.Client_message.Query.New_player dcs.me.id in
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             model))
      helper_func
      graph
  in
  let%sub { current_phase; me; all_trades; _ } = default_client_state in
  let%sub { cash; _ } = me in
  match%sub current_phase with
  | Current_phase.Playing ->
    let%arr all_trades
    and cash
    and exchange = Exchange.updated_orders me graph in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
      [ body cash; exchange; Trade_history.body all_trades ]
  | Current_phase.Enter_user -> Bonsai.return (Username.body ())
  | _ -> Bonsai.return {%html|<p>Waiting for others...</p>|}
;;

(* Entry point *)
let () = Bonsai_web.Start.start (fun graph -> page graph)
