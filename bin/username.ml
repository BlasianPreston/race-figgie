open! Core
open! Bonsai
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml
open! Race_figgie

let body () =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "username_page" ] ]
    [ Vdom.Node.h1
        ~attrs:[ Vdom.Attr.classes [ "username_title" ] ]
        [ Vdom.Node.text "Race Figgie" ]
    ; Vdom.Node.form
        ~attrs:[ Vdom.Attr.classes [ "username_form" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.classes [ "username_input" ]
              ; Vdom.Attr.placeholder "Enter Username"
              ]
            ()
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.type_ "submit"
              ; Vdom.Attr.classes [ "username_submit" ]
              ]
            [ Vdom.Node.text "Submit" ]
        ]
    ]
;;
