open! Core
open Async
open Race_figgie

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

let change_game_phase (game_state : Game_state.t ref) (phase : Current_phase.t) =
  game_state := { !game_state with current_phase = phase }
;;

let reset (game_state : Game_state.t ref) =
  let new_player_map =
    Map.map !game_state.players ~f:(fun player ->
      Player.create player.id)
  in
  game_state
  := { (Game_state.empty ()) with players = new_player_map };
  return ()
;;

let phase (game_state : Game_state.t ref) (phase_to_change_to : Current_phase.t)
  =
  change_game_phase game_state phase_to_change_to;
  let%bind () = sleep (Game_phase.to_duration phase_to_change_to) in
  return ()
;;

let remove_one_racer_from_list list racer =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t ->
      if Game_state.Racer.equal h racer
      then List.rev_append acc t
      else aux (h :: acc) t
  in
  aux [] list
;;

let get_best_bid_order ~(bid_order_list : Game_state.Order.t list) =
  List.filter_map bid_order_list ~f:(fun { player_id; price; _ } ->
    Option.map price ~f:(fun p -> p, player_id))
  |> List.max_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)
;;

let get_best_ask_order ~(ask_order_list : Game_state.Order.t list) =
  List.filter_map ask_order_list ~f:(fun { player_id; price; _ } ->
    Option.map price ~f:(fun p -> p, player_id))
  |> List.min_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)
;;

let rec remove_player_order_from_order_list
  ~player
  ~(order_list : Game_state.Order.t list)
  =
  match order_list with
  | [] -> []
  | current_order :: rest ->
    (match String.equal current_order.player_id player with
     | true -> remove_player_order_from_order_list ~player ~order_list:rest
     | false ->
       [ current_order ]
       @ remove_player_order_from_order_list ~player ~order_list:rest)
;;

let update_state_on_trade
  (state : Game_state.State.t)
  ~bidder
  ~trade_price
  ~bid
  ~asker
  ~ask_order_list
  ~bid_order_list
  ~(racer_traded : Game_state.Racer.t)
  =
  let updated_bid_order_list =
    remove_player_order_from_order_list
      ~player:bidder
      ~order_list:bid_order_list
  in
  let updated_ask_order_list =
    remove_player_order_from_order_list
      ~player:asker
      ~order_list:ask_order_list
  in
  let players_lst = Map.to_alist state.players in
  let askers_player_record =
    List.find_exn players_lst ~f:(fun (_, player) ->
      String.equal player.id asker)
  in
  let _, askers_player = askers_player_record in
  let updated_asker =
    { askers_player with
      cash = askers_player.cash + trade_price
    ; holdings =
        remove_one_racer_from_list askers_player.holdings racer_traded
    }
  in
  let players_lst = Map.to_alist state.players in
  let bidders_player_record =
    List.find_exn players_lst ~f:(fun (_, player) ->
      String.equal player.id asker)
  in
  let _, bidders_player = bidders_player_record in
  let updated_bidder =
    { bidders_player with
      cash = bidders_player.cash + (bid - trade_price)
    ; holdings = racer_traded :: bidders_player.holdings
    }
  in
  let updated_players =
    let players_lst = Map.to_alist state.players in
    Map.of_alist_exn
      (module String)
      (List.map players_lst ~f:(fun (name, p) ->
         if String.equal p.id asker
         then asker, updated_asker
         else if String.equal p.id bidder
         then bidder, updated_bidder
         else name, p))
  in
  (* Update the bids map *)
  let updated_bids =
    Map.add_exn ~key:racer_traded ~data:updated_bid_order_list state.bids
  in
  (* Update the asks map *)
  let updated_asks =
    Map.add_exn ~key:racer_traded ~data:updated_ask_order_list state.asks
  in
  let updated_fills =
    Game_state.Fill.create bidder asker racer_traded trade_price
    :: state.filled_orders
  in
  Game_state.State.update
  ~current_state:state.current_state
    ~players:updated_players
    ~bids:updated_bids
    ~asks:updated_asks
    ~filled_orders:updated_fills
    ~race_positions:state.race_positions
    ~winner:state.winner
;;

let check_for_trades_given_racer
  (state : Game_state.State.t)
  ~(racer : Game_state.Racer.t)
  =
  let bids = state.bids in
  let asks = state.asks in
  let racer_bids = Map.find bids racer in
  let racer_asks = Map.find asks racer in
  match racer_bids, racer_asks with
  | None, _ | _, None -> state
  | Some bid_order_list, Some ask_order_list ->
    (match
       get_best_bid_order ~bid_order_list, get_best_ask_order ~ask_order_list
     with
     | None, _ | _, None -> state
     | Some (best_bid, bidder), Some (best_ask, asker) ->
       (match best_bid >= best_ask with
        | false -> state
        | true ->
          update_state_on_trade
            state
            ~bidder
            ~trade_price:best_ask
            ~bid:best_bid
            ~asker
            ~ask_order_list
            ~bid_order_list
            ~racer_traded:racer))
;;

let match_orders (state : Game_state.State.t) =
  (* Match highest bid with lowest ask if bid >= ask *)
  (* Update players' holdings and cash accordingly *)
  check_for_trades_given_racer state ~racer:Red
  |> check_for_trades_given_racer ~racer:Green
  |> check_for_trades_given_racer ~racer:Yellow
  |> check_for_trades_given_racer ~racer:Blue
;;

let check_winner (game_state : Game_state.State.t ref) =
  let state = !game_state in
  let racers = state.race_positions in
  let winning_racer = List.filter racers ~f:(fun (_, _, distance) -> if distance >= 500 then true else false) in
  match List.is_empty winning_racer with
  | true -> ()
  | false -> match List.hd_exn winning_racer with
  | racer, _, _ -> game_state := (Game_state.State.set_winner state (Some racer)) 

let compute_round_results (authoritative_game_state : Game_state.State.t ref) =
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
