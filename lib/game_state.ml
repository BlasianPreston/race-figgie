open! Core
include Map.Make (Racer)

type position = int
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

let empty () =
  { current_phase = Waiting
  ; players = String.Map.empty
  ; bids = Racer.Map.empty
  ; asks = Racer.Map.empty
  ; filled_orders = []
  ; race_positions = []
  ; winner = None
  }
;;

let racer_frequency_map (lst : 'a list) : ('a, int, _) Map.t =
  List.fold
    lst
    ~init:(Map.empty (module Racer))
    ~f:(fun acc elt ->
      Map.update acc elt ~f:(function None -> 1 | Some count -> count + 1))
;;

let get_client_state_from_name (t : t) (name : string) : Client_state.t =
  let current_phase = t.current_phase in
  let players =
    t.players |> Map.to_alist |> List.map ~f:(fun (_, player) -> player)
  in
  let all_trades = t.filled_orders in
  let ready_players = players in
  let me =
    match Map.find t.players name with
    | None -> failwith "Player not found"
    | Some player -> player
  in
  let holdings = me.holdings in
  let frequency_map = racer_frequency_map holdings in
  let frequency_lst =
    List.map (Map.to_alist frequency_map) ~f:(fun (_, freq) -> freq)
  in
  let highest_freq_opt = List.max_elt frequency_lst ~compare:Int.compare in
  let highest_freq = match highest_freq_opt with None -> 0 | Some x -> x in
  let shown_racers =
    List.filter
      [ Racer.Red; Racer.Yellow; Racer.Blue; Racer.Green ]
      ~f:(fun color ->
        if Int.equal (Map.find_exn frequency_map color) highest_freq
        then true
        else false)
  in
  { current_phase; all_trades; players; ready_players; me; shown_racers }
;;

let name_taken t name =
  let players = t.players in
  match Map.find players name with
  | Some _ -> true
  | None -> false

let add_player t name =
  let players = t.players in
  match Map.find players name with
  | None ->
    { t with
      players = Map.add_exn players ~key:name ~data:(Player.create name)
    }
  | Some _ -> failwith "Player already exists"
;;

let initialize_racers t =
  let racers = [(Racer.Red, 0, 0); (Racer.Yellow, 0, 0); (Racer.Blue, 0, 0); (Racer.Green, 0, 0)] in
  { t with race_positions = racers}

let rec shuffle = function
  | [] -> []
  | [ single ] -> [ single ]
  | list ->
    let before, after =
      List.partition_tf ~f:(fun _ -> Random.bool ()) list
    in
    List.rev_append (shuffle before) (shuffle after)
;;

let distribute lst (n : int) =
  let chunk_size = List.length lst / n in
  let rec split acc current lst count =
    match lst, count with
    | [], _ -> List.rev (List.rev current :: acc)
    | x :: xs, 1 -> split (List.rev (x :: current) :: acc) [] xs chunk_size
    | x :: xs, _ -> split acc (x :: current) xs (count - 1)
  in
  split [] [] lst chunk_size
;;

let add_hands_to_players t =
  let players = t.players in
  let deck =
    List.init 10 ~f:(fun _ -> Racer.Red)
    @ List.init 10 ~f:(fun _ -> Racer.Blue)
    @ List.init 10 ~f:(fun _ -> Racer.Green)
    @ List.init 10 ~f:(fun _ -> Racer.Yellow)
  in
  let shuffled_deck = shuffle deck in
  let groups = distribute shuffled_deck 4 in
  let groups =
    List.filter ~f:(fun lst -> not (List.length lst = 0)) groups
  in
  let player_lst = Map.to_alist players in
  let players_with_cards =
    Map.of_alist_exn
      (module String)
      (List.map2_exn
         ~f:(fun (name, player) cards ->
           name, { player with holdings = cards })
         player_lst
         groups)
  in
  { current_phase = t.current_phase
  ; players = players_with_cards
  ; bids = t.bids
  ; asks = t.asks
  ; filled_orders = t.filled_orders
  ; race_positions = t.race_positions
  ; winner = None
  }
;;

let add_player_and_possibly_add_hand t name =
  let current_players = t.players in
  let all_players = Map.add_exn ~key:name ~data:(Player.create name) current_players in
  let new_state = {t with players=all_players} in
  if Map.length all_players = 4 then add_hands_to_players new_state else new_state

let create
  ~current_phase
  ~players
  ~bids
  ~asks
  ~filled_orders
  ~race_positions
  ~winner
  =
  let state =
    { current_phase
    ; players
    ; bids
    ; asks
    ; filled_orders
    ; race_positions
    ; winner
    }
  in
  add_hands_to_players state
;;

let update
  ~current_phase
  ~players
  ~bids
  ~asks
  ~filled_orders
  ~race_positions
  ~winner
  =
  { current_phase
  ; players
  ; bids
  ; asks
  ; filled_orders
  ; race_positions
  ; winner
  }
;;

let set_winner state winner = { state with winner }

let update_positions t =
  let positions = t.race_positions in
  let race_positions =
    List.map
      ~f:(function
        | racer, (position : position), velocity ->
          if position + velocity < 0
          then racer, 0, velocity
          else racer, position + velocity, velocity)
      positions
  in
  { current_phase = t.current_phase
  ; players = t.players
  ; bids = t.bids
  ; asks = t.asks
  ; filled_orders = t.filled_orders
  ; race_positions
  ; winner = t.winner
  }
;;

let update_velocities t =
  let positions = t.race_positions in
  let race_positions =
    List.map
      ~f:(function
        | racer, (position : position), _ ->
          let () = Random.self_init () in
          let magnitude = 1 + Random.int 10 in
          let sign = if Random.int 10 = 0 then -1 else 1 in
          let new_velocity = sign * magnitude in
          racer, position, new_velocity)
      positions
  in
  { current_phase = t.current_phase
  ; players = t.players
  ; bids = t.bids
  ; asks = t.asks
  ; filled_orders = t.filled_orders
  ; race_positions
  ; winner = t.winner
  }
;;

let _use_functions x y z a =
  let e = Racer.equal x y in
  let s = add_player z a in
  ignore e;
  ignore s
;;
