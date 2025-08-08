open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml
open! Race_figgie

let body best_orders =
  let children =
    List.map best_orders ~f:(fun (racer, (best_bid, best_ask)) ->
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "racer_best_order" ] ]
        [ Vdom.Node.img
            ~attrs:
              [ Vdom.Attr.classes [ "orders_img" ]
              ; Vdom.Attr.src (Racer.to_img racer)
              ]
            ()
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.classes [ "player_orders" ] ]
            [ Vdom.Node.h3 [ Vdom.Node.text ("Best Bid: " ^ best_bid) ]
            ; Vdom.Node.h3 [ Vdom.Node.text ("Best Ask: " ^ best_ask) ]
            ]
        ])
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "best_orders_div" ] ]
    children
;;
