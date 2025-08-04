open! Core
open! Async_kernel
open! Async_rpc_kernel
open Bonsai_web
open Bonsai.Let_syntax
open! Race_figgie

(* Dummy client state *)
let dummy_client_state : Client_state.t Bonsai.t =
  Bonsai.return
    { current_phase = Current_phase.Playing
    ; all_trades = []
    ; players = []
    ; ready_players = []
    ; shown_racers = []
    ; me =
        { id = "joseph"
        ; holdings = []
        ; cash = 400
        }
    }

(* Page rendering logic based on phase *)
let page (client_state : Client_state.t Bonsai.t) (_graph : Bonsai.Private.Reveal.Magic.t Bonsai.Private.Graph.t) =
  let%sub current_phase = Bonsai.Value.map client_state ~f:(fun s -> s.current_phase) |> Bonsai.pure in
  let%sub player_id = Bonsai.map client_state ~f:(fun s -> s.me.id) |> Bonsai.pure in
  match%sub current_phase with
  | Current_phase.Playing ->
    let%arr player_id = player_id in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
      [ Race.body ()
      ; Exchange.serve_body ~player_id
      ; Trade_history.body () ]
  | _ ->
    Bonsai.return
      {%html|<p>Waiting for others...</p>|}

(* Entry point *)
let () =
  Bonsai_web.Start.start
    (fun graph -> page dummy_client_state graph)
