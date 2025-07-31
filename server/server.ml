open! Core
open Async

let handle_client_requesting_client_state
  (query : Rpcs.Poll_client_state.Query.t)
  (authoritative_game_state : Game_state.t ref)
  =
  Game_state.get_client_state_from_name !authoritative_game_state query.name
;;

let sleep (seconds : int) =
  let%bind () = Clock_ns.after (Time_ns.Span.of_int_sec seconds) in
  return ()
;;

let change_game_phase
  (authoritative_game_state : Game_state.t ref)
  (phase : Game_phase.t)
  =
  authoritative_game_state
  := { !authoritative_game_state with
       current_phase = phase
     ; round_start = Time_ns.now ()
     }
;;

let reset (authoritative_game_state : Game_state.t ref) =
  let new_player_map =
    Map.map !authoritative_game_state.players ~f:(fun player ->
      Player.new_player player.name)
  in
  authoritative_game_state
  := { (Game_state.create_empty_game ()) with players = new_player_map };
  return ()
;;

let phase
  (authoritative_game_state : Game_state.t ref)
  (phase_to_change_to : Game_phase.t)
  =
  change_game_phase authoritative_game_state phase_to_change_to;
  let%bind () = sleep (Game_phase.to_duration phase_to_change_to) in
  return ()
;;

let update_player_item_choices_and_round
  (authoritative_game_state : Game_state.t ref)
  (new_round : int)
  =
  let players_with_item_blockers_used_on =
    List.filter_map
      !authoritative_game_state.actions_taken_in_round
      ~f:(fun (action : Action.t) ->
        match action.item_used with
        | Item_blocker -> Some action.recipient
        | Observer | Medical_kit _ | Poisonous_dart _ | Pocket_knife _
        | Gamblers_potion _ ->
          None)
  in
  let new_item_choices =
    Map.mapi
      !authoritative_game_state.players
      ~f:(fun ~key ~data ->
        let player_name = key in 
        ignore data;
        if List.mem players_with_item_blockers_used_on player_name ~equal:String.equal
        then None
        else Some (Item.get_two_random_items_no_duplicates ()))
  in
  authoritative_game_state
  := { !authoritative_game_state with
       item_choices_by_user = new_item_choices
     ; actions_taken_in_round = []
     ; current_round = new_round
     }
;;

let compute_round_results (authoritative_game_state : Game_state.t ref) =
  authoritative_game_state
  := Game_state.apply_actions_taken !authoritative_game_state
     |> Game_state.compile_all_elimination_results
;;

let rec handle_round
  (authoritative_game_state : Game_state.t ref)
  ~(round : int)
  : unit Deferred.t
  =
  update_player_item_choices_and_round authoritative_game_state round;
  let%bind () = phase authoritative_game_state Item_selection in
  let%bind () = phase authoritative_game_state Negotiation in
  let%bind () = phase authoritative_game_state Item_usage in
  compute_round_results authoritative_game_state;
  let%bind () = phase authoritative_game_state Round_results in
  let players_left = Game_state.players_left !authoritative_game_state in
  if players_left > 0 && round < 10
  then handle_round authoritative_game_state ~round:(round + 1)
  else (
    let%bind () = phase authoritative_game_state Game_results in
    reset authoritative_game_state)
;;

let everyone_is_ready (authoritative_game_state : Game_state.t ref) =
  List.length !authoritative_game_state.ready_players
  = Map.length !authoritative_game_state.players
;;

let start_game (authoritative_game_state : Game_state.t ref) =
  let%bind () = phase authoritative_game_state Rules in
  handle_round authoritative_game_state ~round:1
;;

let handle_ready_message
  (authoritative_game_state : Game_state.t ref)
  (query : Rpcs.Client_message.Ready_status_change.t)
  : Rpcs.Client_message.Response.t
  =
  match Game_state.name_taken !authoritative_game_state query.name with
  | true ->
    authoritative_game_state
    := Game_state.ready_player !authoritative_game_state query;
    if everyone_is_ready authoritative_game_state
    then start_game authoritative_game_state |> don't_wait_for;
    Ok "OK"
  | false -> Error "Player name isn't registered"
;;

let handle_item_selection
  (authoritative_game_state : Game_state.t ref)
  (query : Rpcs.Client_message.Item_selection.t)
  : Rpcs.Client_message.Response.t
  =
  match
    Game_phase.equal
      Game_phase.Item_selection
      !authoritative_game_state.current_phase
  with
  | true ->
    authoritative_game_state
    := Game_state.add_item_to_inventory !authoritative_game_state query;
    Ok "OK"
  | false -> Error "It is not currently the item selection phase"
;;

let handle_message
  (authoritative_game_state : Game_state.t ref)
  (message : Message.t)
  : Rpcs.Client_message.Response.t
  =
  match
    Game_phase.equal
      Game_phase.Negotiation
      !authoritative_game_state.current_phase
  with
  | true ->
    authoritative_game_state
    := Game_state.add_message !authoritative_game_state message;
    Ok "OK"
  | false -> Error "It is not currently the negotiation phase"
;;

let handle_item_used
  (authoritative_game_state : Game_state.t ref)
  (action : Action.t)
  : Rpcs.Client_message.Response.t
  =
  match
    Game_phase.equal
      Game_phase.Item_usage
      !authoritative_game_state.current_phase
  with
  | true ->
    authoritative_game_state
    := Game_state.add_action !authoritative_game_state action;
    Ok "OK"
  | false -> Error "It is not currently the item usage phase"
;;

let handle_new_player
  (authoritative_game_state : Game_state.t ref)
  (name : string)
  : Rpcs.Client_message.Response.t
  =
  match Game_state.name_taken !authoritative_game_state name with
  | true -> Error "Name already taken"
  | false ->
    authoritative_game_state
    := Game_state.add_player
         !authoritative_game_state
         (Player.new_player name);
    Ok "OK"
;;

let handle_client_message
  (query : Rpcs.Client_message.Query.t)
  (authoritative_game_state : Game_state.t ref)
  =
  match query with
  | New_player name -> handle_new_player authoritative_game_state name
  | Ready_status_change status_change ->
    handle_ready_message authoritative_game_state status_change
  | Item_selection item_selection ->
    handle_item_selection authoritative_game_state item_selection
  | Chat_message message -> handle_message authoritative_game_state message
  | Item_used action -> handle_item_used authoritative_game_state action
;;

let web_handler =
  Cohttp_static_handler.Single_page_handler.create_handler
    (Cohttp_static_handler.Single_page_handler.default_with_body_div
       ~div_id:"app")
    ~title:"Hangry Squid"
    ~on_unknown_url:`Not_found
    ~assets:
      [ Cohttp_static_handler.Asset.local
          Cohttp_static_handler.Asset.Kind.javascript
          (Cohttp_static_handler.Asset.What_to_serve.file
             ~relative_to:`Exe
             ~path:"../client/main.bc.js")
      ; Cohttp_static_handler.Asset.local
          Cohttp_static_handler.Asset.Kind.css
          (Cohttp_static_handler.Asset.What_to_serve.file
             ~relative_to:`Exe
             ~path:"../client/style.css")
      ]
;;

let start_server host_and_port authoritative_game_state =
  let listen_at =
    Tcp.Where_to_listen.create
      ~socket_type:Socket.Type.tcp
      ~address:
        (Socket.Address.Inet.create
           (Unix.Inet_addr.of_string (Host_and_port.host host_and_port))
           ~port:(Host_and_port.port host_and_port))
      ~listening_on:(function `Inet (_, port) -> port)
  in
  let server =
    Rpc_websocket.Rpc.serve
      ~where_to_listen:listen_at
      ~initial_connection_state:(fun () _ _ conn -> (), conn)
      ~http_handler:(fun _ -> web_handler)
      ~implementations:
        (Rpc.Implementations.create_exn
           ~on_exception:Rpc.On_exception.Close_connection
           ~on_unknown_rpc:`Close_connection
           ~implementations:
             [ Rpc.Rpc.implement Rpcs.Client_message.rpc (fun _ query ->
                 return
                   (handle_client_message query authoritative_game_state))
             ; Polling_state_rpc.implement
                 ~on_client_and_server_out_of_sync:(fun _ -> ())
                 Rpcs.Poll_client_state.rpc
                 (fun _ query ->
                   return
                     (handle_client_requesting_client_state
                        query
                        authoritative_game_state))
             ])
      ()
  in
  let%bind server in
  let%bind () = Cohttp_async.Server.close_finished server in
  return ()
;;

let start_server_command =
  Command.async
    ~summary:"Start a game server"
    (let%map_open.Command host_and_server_port =
       flag "address" (required host_and_port) ~doc:"<host>:<port>"
     in
     let authoritative_game_state = ref (Game_state.create_empty_game ()) in
     fun () -> start_server host_and_server_port authoritative_game_state)
;;