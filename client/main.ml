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

let loading_page = Vdom.Node.div ~attrs:[Vdom.Attr.classes ["loading"]] [Vdom.Node.p [Vdom.Node.text "Loading..."]]


let page_with_state (client_state : Client_state.t Bonsai.t) (local_ graph) =
  let%arr exchange = Exchange.updated_orders client_state graph in
  Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
      [ body 69; exchange; Trade_history.body [Fill.create "Preston" "Joseph" Racer.Blue 10
      ; Fill.create "Bari" "Fahim" Racer.Red 10] ]

let render_game_page name (local_ graph) =
  let client_state : Client_state.t option Bonsai.t =
    let query =
      let%arr name in
      { Rpcs.Poll_client_state.Query.name }
    in
    let result =
      Rpc_effect.Polling_state_rpc.poll
        Rpcs.Poll_client_state.rpc
        ~equal_query:[%equal: Rpcs.Poll_client_state.Query.t]
        ~every:(Bonsai.return (Time_ns.Span.of_sec 0.1))
        ~where_to_connect:
          (Bonsai.return
             (Rpc_effect.Where_to_connect.self
                ~on_conn_failure:
                  Rpc_effect.On_conn_failure.Surface_error_to_rpc
                ()))
        query
        graph
    in
    let%arr result in
    (* do something with Error types? *)
    match result.last_ok_response with
    | None -> None
    | Some (_query, resp) -> Some resp
  in
  match%sub client_state with
  | None -> Bonsai.return loading_page
  | Some client_state -> page_with_state client_state graph
;;

let render_landing_page update_join_game_state error =
  let%arr update_join_game_state and error in
  Username.body update_join_game_state error
;;

let serve_route (local_ graph) =
  let dispatcher =
    Rpc_effect.Rpc.dispatcher
      Rpcs.Client_message.rpc
      graph
      ~where_to_connect:
        (Bonsai.return
           (Rpc_effect.Where_to_connect.self
              ~on_conn_failure:
                Rpc_effect.On_conn_failure.Surface_error_to_rpc
              ()))
  in
  let join_game_state, update_join_game_state =
    Bonsai.state_machine_with_input
      ~default_model:(Entering_name ("", None) : Landing_state.t)
      ~apply_action:(fun ctx input (model : Landing_state.t) action ->
        match input with
        | Active dispatcher ->
          (match action with
           | `Ack_join accepted_name -> Accepted_name accepted_name
           | `Failed_join error ->
             Entering_name (Landing_state.name model, Some error)
           | `Update_name new_name -> Entering_name (new_name, None)
           | `Try_to_join_game ->
              if String.equal "" (Landing_state.name model) then model else (
             let player_name_query =
               Rpcs.Client_message.Query.New_player
                 (Landing_state.name model)
             in
             let my_new_effect =
               let%bind.Effect result = dispatcher player_name_query in
               match result with
               | Error error ->
                 Bonsai.Apply_action_context.inject
                   ctx
                   (`Failed_join (Error.to_string_hum error))
               | Ok resp ->
                 (match resp with
                  | Ok _ ->
                    Bonsai.Apply_action_context.inject
                      ctx
                      (`Ack_join (Landing_state.name model))
                  | Error error_message ->
                    Bonsai.Apply_action_context.inject
                      ctx
                      (`Failed_join error_message))
             in
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               my_new_effect;
             model))
        | Inactive -> model)
      dispatcher
      graph
  in
  match%sub join_game_state with
  | Accepted_name name -> render_game_page name graph
  | Entering_name (_, error) ->
    render_landing_page update_join_game_state error
;;

let () = Bonsai_web.Start.start serve_route
