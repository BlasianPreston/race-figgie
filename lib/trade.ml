open! Core

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
  ignore bid;
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
  let bidders_player_record =
    List.find_exn players_lst ~f:(fun (_, player) ->
      String.equal player.id bidder)
  in
  let _, bidders_player = bidders_player_record in
  let updated_bidder =
    { bidders_player with
      cash = bidders_player.cash - trade_price
    ; holdings = racer_traded :: bidders_player.holdings
    }
  in
  let updated_players =
    let players_lst = Map.to_alist state.players in
    Map.of_alist_exn
      (module String)
      (List.map players_lst ~f:(fun (name, p) ->
         if String.equal p.id asker
         then name, updated_asker
         else if String.equal p.id bidder
         then name, updated_bidder
         else name, p))
  in
  print_endline "so far so good";
  (* Update the bids map *)
  let updated_bids =
    Map.set state.bids ~key:racer_traded ~data:updated_bid_order_list
  in
  print_endline "bids updated!";
  let updated_asks =
    Map.set state.asks ~key:racer_traded ~data:updated_ask_order_list
  in
  print_endline "asks_updated!";
  let updated_fills =
    Fill.create bidder asker racer_traded trade_price :: state.filled_orders
  in
  print_s [%sexp (updated_fills : Fill.t list)];
  print_endline "Fills have been updated!";
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

let get_current_best_prices_for_racer (state : Game_state.t) ~racer =
  let racer_bids = Map.find state.bids racer in
  let racer_asks = Map.find state.asks racer in
  let best_bid =
    match racer_bids with
    | None -> "X"
    | Some bid_list ->
      let bid_opt = get_best_bid_order ~bid_order_list:bid_list in
      (match bid_opt with None -> "X" | Some (bid, _) -> Int.to_string bid)
  in
  let best_ask =
    match racer_asks with
    | None -> "X"
    | Some ask_list ->
      let ask_opt = get_best_ask_order ~ask_order_list:ask_list in
      (match ask_opt with None -> "X" | Some (ask, _) -> Int.to_string ask)
  in
  best_bid, best_ask
;;

let get_best_bids_and_asks state =
  let best_red = get_current_best_prices_for_racer state ~racer:Red in
  let best_yellow = get_current_best_prices_for_racer state ~racer:Yellow in
  let best_green = get_current_best_prices_for_racer state ~racer:Green in
  let best_blue = get_current_best_prices_for_racer state ~racer:Blue in
  Map.of_alist_exn
    (module Racer)
    [ Racer.Red, best_red
    ; Racer.Yellow, best_yellow
    ; Racer.Blue, best_blue
    ; Racer.Green, best_green
    ]
;;
