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

let body () =
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
    ; buying_power 400
    ]
;;


(* Dummy client state *)
let dummy_client_state : Client_state.t Bonsai.t =
  Bonsai.return
    { Client_state.current_phase = Current_phase.Playing
    ; all_trades = []
    ; players = []
    ; ready_players = []
    ; shown_racers = []
    ; me = { id = "joseph"; holdings = []; cash = 400 }
    } 
;;

let page
  (client_state : Client_state.t Bonsai.t)
  (local_ _graph)
  =
  let%sub {current_phase;me;_} =
    client_state
  in
  let%sub {id;_} = me
  in
  match%sub current_phase with
  | Current_phase.Playing ->
    let%arr id in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
      [ body ()
      ; Exchange.serve_body ~player_id:id
      ; Trade_history.body () ]
  | _ -> Bonsai.return {%html|<p>Waiting for others...</p>|}
;;

(* Entry point *)
let () = Bonsai_web.Start.start (fun graph -> page dummy_client_state graph)