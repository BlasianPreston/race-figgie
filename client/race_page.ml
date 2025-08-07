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
        [ Vdom.Node.text ("Buying Power: $" ^ Int.to_string bp) ]
    ]
;;

let positions (race_positions : (Racer.t * int * int) list) : Vdom.Node.t =
  let children =
    List.concat_map race_positions ~f:(fun (racer, position, _) ->
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.classes [ "race_positions" ] ]
          [ Vdom.Node.h3
              [ Vdom.Node.img
                  ~attrs:
                    [ Vdom.Attr.src (Racer.to_img racer)
                    ; Vdom.Attr.classes [ "trade_img" ]
                    ]
                  ()
              ; Vdom.Node.text (Int.to_string position)
              ]
          ]
      ])
  in
  Vdom.Node.div children
;;

let body (race_positions : (Racer.t * int * int) list) bp =
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
    ; positions race_positions
    ; buying_power bp
    ]
;;
