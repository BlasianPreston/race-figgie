open! Core
open! Async_kernel
open! Async_rpc_kernel
open Bonsai_web
open Bonsai.Let_syntax
open! Race_figgie

let loading_page =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:[ Vdom.Attr.classes [ "loading" ] ]
       [ Vdom.Node.p [ Vdom.Node.text "Loading..." ] ])
;;

let page_with_state (client_state : Client_state.t Bonsai.t) (local_ graph) =
  let%sub { me; current_phase; race_positions; all_trades; _ } = client_state in
  match%sub current_phase with
  | Waiting -> loading_page
  | Playing ->
<<<<<<< HEAD
    let%sub { cash; _ } = me in
    let%arr all_trades
    and exchange = Exchange.updated_orders client_state graph
    and cash in
=======
    let%sub {cash ; _} = me in
    
    let%arr all_trades and exchange = Exchange.updated_orders client_state graph and cash = cash and race_positions in
    let race = Race_page.body race_positions cash in
    let trade_history = Trade_history.body all_trades in
>>>>>>> a2eeddb0f5b16d9b64d901c53b133a74350c9ef4
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
      [ race; exchange; trade_history ]
  | _ -> loading_page
;;

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
    ignore
      (match result.last_error with
       | None -> ()
       | Some (_, err) -> print_s [%sexp (err : Error.t)]);
    match result.last_ok_response with
    | None -> None
    | Some (_query, resp) -> Some resp
  in
  match%sub client_state with
  | None -> loading_page
  | Some client_state -> page_with_state client_state graph
;;

let render_landing_page update_join_game_state error =
  let%arr update_join_game_state and error in
  Username.body update_join_game_state error
;;

let serve_route (local_ graph) =
  let () = print_endline "Rendering serve_route" in
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
           | `Update_name new_name ->
             print_endline "Entering name";
             Entering_name (new_name, None)
           | `Try_to_join_game ->
             print_s [%sexp (model : Landing_state.t)];
             if String.equal "" (Landing_state.name model)
             then model
             else (
               let player_name_query =
                 Rpcs.Client_message.Query.New_player
                   (Landing_state.name model)
               in
               let my_new_effect =
                 let%bind.Effect result = dispatcher player_name_query in
                 print_endline " we are dispatching";
                 match result with
                 | Error error ->
                   print_endline "1";
                   Bonsai.Apply_action_context.inject
                     ctx
                     (`Failed_join (Error.to_string_hum error))
                 | Ok resp ->
                   print_endline "2";
                   print_s [%sexp (resp : (string, string) result)];
                   (match resp with
                    | Ok msg ->
                      if not (String.equal "Ready" msg)
                      then (
                        print_endline "Not everyone ready";
                        Bonsai.Apply_action_context.inject
                          ctx
                          (`Ack_join (Landing_state.name model)))
                      else (
                        let everyone_ready_query =
                          Rpcs.Client_message.Query.Everyone_ready
                        in
                        let everyone_ready_effect =
                          let%bind.Effect ready_result =
                            dispatcher everyone_ready_query
                          in
                          print_endline "hiii ";
                          print_s
                            [%sexp
                              (ready_result
                               : ((string, string) result, Error.t) result)];
                          match ready_result with
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
                          everyone_ready_effect;
                        Bonsai.Apply_action_context.inject
                          ctx
                          (`Ack_join (Landing_state.name model)))
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
