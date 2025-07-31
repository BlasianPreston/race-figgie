open! Core

module Racer = struct
  type t =
    | Red
    | Yellow
    | Blue
    | Green
  [@@deriving equal, compare, hash, sexp]

  let to_string = function
    | Red -> "Red"
    | Blue -> "Blue"
    | Green -> "Green"
    | Yellow -> "Yellow"
  ;;

  let of_string = function
    | "Red" -> Red
    | "Blue" -> Blue
    | "Green" -> Green
    | "Yellow" -> Yellow
    | _ -> Red
  ;;

  let to_img = function
    | Red -> "../images/pink_character.png"
    | Blue -> "../images/updated_blue_character.png"
    | Green -> "../images/green_character.png"
    | Yellow -> "../images/updated_yellow_character.png"
  ;;

  let compare x y = String.compare (to_string x) (to_string y)
  let sexp_of_t t = String.sexp_of_t (to_string t)
  let t_of_sexp sexp = of_string (String.t_of_sexp sexp)

  include Comparable.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)
end

type position = int
type velocity = int

module Player = struct
  type t =
    { id : string
    ; holdings : Racer.t list
    ; cash : int
    }
  [@@deriving compare, hash, sexp_of]

  let create name = { id = name; holdings = []; cash = 400 }
  let create_with_holdings id holdings = { id; holdings; cash = 400 }
end

type order_type =
  | Bid
  | Ask
[@@deriving equal, compare, hash, sexp]

module Order = struct
  type t =
    { player_id : string
    ; racer : Racer.t
    ; price : int option
    ; order_type : order_type
    }
  [@@deriving equal, compare, hash, sexp]

  let create ~player_id ~racer ~(price : int option) ~order_type =
    { player_id; racer; price; order_type }
  ;;

  let is_no_order t = match t.price with None -> true | Some _ -> false
end

type trade =
  { buyer : string
  ; seller : string
  ; racer : Racer.t
  ; price : int
  }

module Fill = struct
  type t =
    { buyer : string
    ; seller : string
    ; racer : Racer.t
    ; price : int
    }

  let create buyer seller racer price = { buyer; seller; racer; price }
end

module State = struct
  include Map.Make (Racer)

  type current_state =
    | Waiting
    | Playing
    | End

  type t =
    { current_state : current_state
    ; players : Player.t String.Map.t
    ; bids : Order.t list Racer.Map.t
    ; asks : Order.t list Racer.Map.t
    ; filled_orders : Fill.t list
    ; race_positions : (Racer.t * position * velocity) list
    ; winner : Racer.t option
    }

  let empty () =
    { current_state = Waiting
    ; players = String.Map.empty
    ; bids = Racer.Map.empty
    ; asks = Racer.Map.empty
    ; filled_orders = []
    ; race_positions = []
    ; winner = None
    }
  ;;

  let add_player t name =
    let players = t.players in
    match Map.find players name with
    | None ->
      { t with
        players = Map.add_exn players ~key:name ~data:(Player.create name)
      }
    | Some _ -> failwith "Player already exists"
  ;;

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
    { current_state = t.current_state
    ; players = players_with_cards
    ; bids = t.bids
    ; asks = t.asks
    ; filled_orders = t.filled_orders
    ; race_positions = t.race_positions
    ; winner = None
    }
  ;;

  let create
    ~current_state
    ~players
    ~bids
    ~asks
    ~filled_orders
    ~race_positions
    ~winner
    =
    let state =
      { current_state
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
    ~current_state
    ~players
    ~bids
    ~asks
    ~filled_orders
    ~race_positions
    ~winner
    =
    { current_state
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
    { current_state = t.current_state
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
    { current_state = t.current_state
    ; players = t.players
    ; bids = t.bids
    ; asks = t.asks
    ; filled_orders = t.filled_orders
    ; race_positions
    ; winner = t.winner
    }
  ;;
end

let _use_functions x y z a =
  let e = Racer.equal x y in
  let s = State.add_player z a in
  ignore e;
  ignore s
;;
