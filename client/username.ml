open! Core
open! Bonsai
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml
open! Race_figgie

let body update_join_game_state error =
  let main_page = Vdom.Node.div
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
              ; Vdom.Attr.on_input (fun _ current_name ->
                  update_join_game_state (`Update_name current_name))
              ; Vdom.Attr.on_keydown (fun event ->
                  match
                    Js_of_ocaml.Dom_html.Keyboard_code.of_event event
                  with
                  | Enter -> update_join_game_state `Try_to_join_game
                  | _ -> Effect.all_unit [])
              ]
            ()
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.type_ "submit"
              ; Vdom.Attr.classes [ "username_submit" ]
              ; Vdom.Attr.on_click (fun _ ->
                  update_join_game_state `Try_to_join_game)
              ]
            [ Vdom.Node.text "Submit" ]
        ]
    ] in
  let error_message =
    let error_text =
      match error with None -> "" | Some error_message -> error_message
    in
    Vdom.Node.div
      ~attrs:[ [%css {|
        color: red;
        |}] ]
      [ Vdom.Node.text error_text ] in
  Vdom.Node.div [main_page; error_message]
;;
