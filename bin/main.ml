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
let dummy_client_state : Client_state.t Bonsai.t =
  Bonsai.return
    { Client_state.current_phase = Current_phase.Enter_user
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
    ; me = { id = "joseph"; holdings = []; cash = 69 }
    }
;;

let apply_client_action (_ctx : (_, _) Bonsai.Apply_action_context.t)
                        (model : Client_state.t)
                        (action : client_action)
  : Client_state.t =
  match action with
  | Set_my_red_bid bid ->
      { model with my_red_bid = bid }
  | Set_my_blue_ask ask ->
      { model with my_blue_ask = ask }
  | Reset -> dummy_client_state
  ;;

let page (local_ _graph) =
  let default_client_state, update_initial_state =
    Bonsai.state_machine
      ~default_model:dummy_client_state
      ~apply_action:




  let%sub { current_phasea
          ; me
          ; all_trades
          ; my_red_bid
          ; my_yellow_bid
          ; my_blue_bid
          ; my_green_bid
          ; my_red_ask
          ; my_yellow_ask
          ; my_blue_ask
          ; my_green_ask
          ; _
          }
    =
    default_client_state
  in
  let%sub { id; cash; _ } = me in
  match%sub current_phase with
  | Current_phase.Playing ->
    let%arr id
    and all_trades
    and cash
    and my_red_bid
    and my_green_ask
    and my_blue_ask
    and my_blue_bid
    and my_green_bid
    and my_red_ask
    and my_yellow_ask
    and my_yellow_bid in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
      [ body cash
      ; Exchange.serve_body
          ~player_id:id
          ~my_red_bid
          ~my_yellow_bid
          ~my_blue_bid
          ~my_green_bid
          ~my_red_ask
          ~my_yellow_ask
          ~my_blue_ask
          ~my_green_ask
      ; Trade_history.body all_trades
      ]
  | Current_phase.Enter_user -> Bonsai.return (Username.body ())
  | _ -> Bonsai.return {%html|<p>Waiting for others...</p>|}
;;

(* Entry point *)
let () = Bonsai_web.Start.start (fun graph -> page graph) 
