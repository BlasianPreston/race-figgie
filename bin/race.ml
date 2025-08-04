open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml

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
