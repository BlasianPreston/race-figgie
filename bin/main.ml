open! Core
open! Bonsai_web

let page () =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:[ Vdom.Attr.classes [ "full_page" ] ]
       [ Race.body (); Exchange.body (); Trade_history.body () ])
;;

let () = Bonsai_web.Start.start (fun _graph -> page ())
