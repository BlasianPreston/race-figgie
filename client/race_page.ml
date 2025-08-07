open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml
open! Race_figgie

let buying_power bp =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "buying_power" ] ]
    [ Vdom.Node.h2
        ~attrs:[ Vdom.Attr.classes [ "buying_power_text" ] ]
        [ Vdom.Node.text ("Cash: $" ^ Int.to_string bp) ]
    ]
;;

let positions (race_positions : (Racer.t * int * int) list) : Vdom.Node.t =
  print_s [%sexp (race_positions : (Racer.t * int * int) list)];
  let children =
    List.concat_map race_positions ~f:(fun (racer, position, _) ->
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.classes [ "race_positions" ] ]
          [ Vdom.Node.img
              ~attrs:
                [ Vdom.Attr.src (Racer.to_img racer)
                ; Vdom.Attr.classes [ "trade_img" ]
                ]
              ()
          ; Vdom.Node.h3 [ Vdom.Node.text (Int.to_string position) ]
          ]
      ])
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "racers_div" ] ] children
;;

let show_holdings (holdings : Racer.t list) =
  let children =
    List.concat_map holdings ~f:(fun racer ->
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.classes [ "race_positions" ] ]
          [ Vdom.Node.img
              ~attrs:
                [ Vdom.Attr.src (Racer.to_img racer)
                ; Vdom.Attr.classes [ "trade_img" ]
                ]
              ()
          ]
      ])
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.classes [ "holdings_div" ] ] children
;;

let body (race_positions : (Racer.t * int * int) list) holdings bp =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "race_page" ] ]
    [ Vdom.Node.h1
        ~attrs:[ Vdom.Attr.classes [ "race_header" ] ]
        [ Vdom.Node.text "Race Figgie" ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "image_div" ] ]
        [ Vdom.Node.img
            ~attrs:
              [ Vdom.Attr.classes [ "race_img" ]
              ; Vdom.Attr.src "../images/race.png"
              ]
            ()
        ]
    ; positions race_positions
    ; show_holdings holdings
    ; buying_power bp
    ]
;;
