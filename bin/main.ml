open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

(* Assuming you have modules: Race, Exchange, Trade_history 
   and Exchange.serve_body : player_id:string -> Vdom.Node.t *)

let page () =
  let player_id = "p1" in  (* Example player id *)

  Bonsai.return
    (Vdom.Node.div
       ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
       [ Race.body ()
       ; Exchange.serve_body ~player_id
       ; Trade_history.body ()
       ])

let () = Bonsai_web.Start.start (fun _graph -> page ())
