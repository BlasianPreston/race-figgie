open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml
open! Race_figgie

let banner () =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "banner" ] ]
    [ Vdom.Node.h1
        ~attrs:[ Vdom.Attr.classes [ "banner_text" ] ]
        [ Vdom.Node.text "Trade History" ]
    ]
;;

let legend () =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "legend" ] ]
    [ Vdom.Node.h3 [ Vdom.Node.text "Buyer" ]
    ; Vdom.Node.h3 [ Vdom.Node.text "Seller" ]
    ; Vdom.Node.h3 [ Vdom.Node.text "Price" ]
    ; Vdom.Node.h3 [ Vdom.Node.text "Racer" ]
    ]
;;

let orders (filled_orders : Fill.t list) =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "trades" ] ]
    (List.concat_map filled_orders ~f:(fun fill ->
       [ Vdom.Node.h3 [ Vdom.Node.text fill.buyer ]
       ; Vdom.Node.h3 [ Vdom.Node.text fill.seller ]
       ; Vdom.Node.h3 [ Vdom.Node.text (Int.to_string fill.price) ]
       ; Vdom.Node.h3
           [ Vdom.Node.img
               ~attrs:
                 [ Vdom.Attr.src (Racer.to_img fill.racer)
                 ; Vdom.Attr.classes [ "trade_img" ]
                 ]
               ()
           ]
       ]))
;;

let body () =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "trade_page" ] ]
    [ banner ()
    ; legend ()
    ; orders
        [ Fill.create "Preston" "Joseph" Racer.Blue 10
        ]
    ]
;;

let serve_body () = Bonsai.return (body ())
