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

let change_game_phase
  (game_state : Game_state.t ref)
  (phase : Current_phase.t)
  =
  game_state := { !game_state with current_phase = phase }
;;

let _reset (game_state : Game_state.t ref) =
  let new_player_map =
    Map.map !game_state.players ~f:(fun player -> Player.create player.id)
  in
  game_state := { (Game_state.empty ()) with players = new_player_map };
  return ()
;;

let remove_one_racer_from_list list racer =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t ->
      if Racer.equal h racer then List.rev_append acc t else aux (h :: acc) t
  in
  aux [] list
;;

let get_best_bid_order ~(bid_order_list : Order.t list) =
  List.filter_map bid_order_list ~f:(fun { player_id; price; _ } ->
    Option.map price ~f:(fun p -> p, player_id))
  |> List.max_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)
;;

let get_best_ask_order ~(ask_order_list : Order.t list) =
  List.filter_map ask_order_list ~f:(fun { player_id; price; _ } ->
    Option.map price ~f:(fun p -> p, player_id))
  |> List.min_elt ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)
;;

let rec remove_player_order_from_order_list
  ~player
  ~(order_list : Order.t list)
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
  (state : Game_state.t)
  ~bidder
  ~trade_price
  ~bid
  ~asker
  ~ask_order_list
  ~bid_order_list
  ~(racer_traded : Racer.t)
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
    Fill.create bidder asker racer_traded trade_price :: state.filled_orders
  in
  Game_state.update
    ~current_phase:state.current_phase
    ~players:updated_players
    ~bids:updated_bids
    ~asks:updated_asks
    ~filled_orders:updated_fills
    ~race_positions:state.race_positions
    ~winner:state.winner
;;

let check_for_trades_given_racer (state : Game_state.t) ~(racer : Racer.t) =
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

let match_orders (state : Game_state.t) =
  (* Match highest bid with lowest ask if bid >= ask *)
  (* Update players' holdings and cash accordingly *)
  check_for_trades_given_racer state ~racer:Red
  |> check_for_trades_given_racer ~racer:Green
  |> check_for_trades_given_racer ~racer:Yellow
  |> check_for_trades_given_racer ~racer:Blue
;;

let _check_winner (game_state : Game_state.t ref) =
  let state = !game_state in
  let racers = state.race_positions in
  let winning_racer =
    List.filter racers ~f:(fun (_, _, distance) ->
      if distance >= 500 then true else false)
  in
  match List.is_empty winning_racer with
  | true -> ()
  | false ->
    (match List.hd_exn winning_racer with
     | racer, _, _ -> game_state := Game_state.set_winner state (Some racer))
;;

let _everyone_is_ready (game_state : Game_state.t ref) =
  Map.length !game_state.players = 4
;;

let start_game (game_state : Game_state.t ref) =
  change_game_phase game_state Current_phase.Playing
;;

let take_money_for_pot (game_state : Game_state.t ref) =
  game_state := Game_state.take_money_for_pot !game_state
;;

let handle_new_player (game_state : Game_state.t ref) name =
  game_state := Game_state.add_player_and_possibly_add_hand !game_state name;
  Ok "Ok"
;;

let handle_order_placed (game_state : Game_state.t ref) (order : Order.t) =
  game_state := Game_state.add_order !game_state order;
  Ok "Ok"
;;

let handle_order_filled (game_state : Game_state.t ref) fill =
  game_state := Game_state.add_fill !game_state fill;
  Ok "Ok"
;;

let handle_client_message
  (query : Rpcs.Client_message.Query.t)
  (game_state : Game_state.t ref)
  =
  match query with
  | New_player name -> handle_new_player game_state name
  | Order_placed order -> handle_order_placed game_state order
  | Order_filled fill -> handle_order_filled game_state fill
;;

let wait_for_winner (game_state : Game_state.t ref) =
  let rec loop passes =
    match !game_state.winner with
    | Some w -> game_state := { !game_state with winner = Some w }
    | None ->
      game_state := Game_state.update_positions !game_state;
      if passes = 5
      then (
        game_state := Game_state.update_velocities !game_state;
        game_state := match_orders !game_state;
        Core_unix.sleep 1;
        loop 1)
      else Core_unix.sleep 1;
      loop (passes + 1)
  in
  loop 1
;;

let compute_round_results (game_state : Game_state.t ref) =
  let players = Map.data !game_state.players in
  (* Determine the goal suite *)
  let goal_suite =
    match !game_state.winner with
    | Some suite -> suite
    | None -> failwith "Goal suite (winner) not set"
  in
  (* Count how many goal suite cards each player has *)
  let player_goal_counts =
    List.map players ~f:(fun player ->
      let count =
        List.count player.holdings ~f:(fun card ->
          Racer.equal card goal_suite)
      in
      player.id, player, count)
  in
  (* Determine the winner — player with most of the goal suite *)
  let winner_id, _, _ =
    List.max_elt player_goal_counts ~compare:(fun (_, _, a) (_, _, b) ->
      Int.compare a b)
    |> Option.value_exn
  in
  (* Apply earnings *)
  let updated_players =
    List.map player_goal_counts ~f:(fun (id, player, count) ->
      if String.equal id winner_id
      then (
        let penalty = (10 - count) * 10 in
        let earnings = 200 - penalty in
        { player with cash = player.cash + earnings })
      else (
        let earnings = count * 10 in
        { player with cash = player.cash + earnings }))
  in
  (* Update map with cleared holdings *)
  let updated_players_assoc =
    List.map updated_players ~f:(fun player ->
      player.id, { player with holdings = [] })
  in
  let updated_player_map = String.Map.of_alist_exn updated_players_assoc in
  game_state := { !game_state with players = updated_player_map }
;;

let end_game (game_state : Game_state.t ref) =
  game_state := { !game_state with current_phase = Current_phase.End }
;;

let _handle_round (game_state : Game_state.t ref) ~(_round : int)
  : unit Deferred.t
  =
  let reset_player_hands = Game_state.reset_hands !game_state in
  game_state := Game_state.add_hands_to_players reset_player_hands;
  take_money_for_pot game_state;
  start_game game_state;
  wait_for_winner game_state;
  compute_round_results game_state;
  end_game game_state;
  let%bind () = sleep 1 in
  return ()
;;

let web_handler =
  Cohttp_static_handler.Single_page_handler.create_handler
    (Cohttp_static_handler.Single_page_handler.default_with_body_div
       ~div_id:"app")
    ~title:"Race Figgie"
    ~on_unknown_url:`Not_found
    ~assets:
      [ Cohttp_static_handler.Asset.local
          Cohttp_static_handler.Asset.Kind.javascript
          (Cohttp_static_handler.Asset.What_to_serve.file
             ~relative_to:`Exe
             ~path:"../bin/main.bc.js")
      ; Cohttp_static_handler.Asset.local
          Cohttp_static_handler.Asset.Kind.css
          (Cohttp_static_handler.Asset.What_to_serve.file
             ~relative_to:`Exe
             ~path:"../bin/styles.css")
      ]
;;

let start_server host_and_port game_state =
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
                 return (handle_client_message query game_state))
             ; Polling_state_rpc.implement
                 ~on_client_and_server_out_of_sync:(fun _ -> ())
                 Rpcs.Poll_client_state.rpc
                 (fun _ query ->
                   return
                     (handle_client_requesting_client_state query game_state))
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
     let game_state = ref (Game_state.empty ()) in
     fun () -> start_server host_and_server_port game_state)
;;
