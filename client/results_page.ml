open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml
open! Race_figgie

let players_view
  (players : Player.t list)
  ~(me : string)
  ~(winner : string)
  ~cash_winner
  : Vdom.Node.t
  =
  let children =
    List.map players ~f:(fun player ->
      let name_style =
        if String.equal player.id me
        then Css_gen.create ~field:"color" ~value:"green"
        else Css_gen.create ~field:"color" ~value:"black"
      in
      let crown =
        if String.equal player.id winner
        then
          Vdom.Node.span
            ~attrs:[ Vdom.Attr.classes [ "crown" ] ]
            [ Vdom.Node.text " 👑" ]
        else Vdom.Node.none
      in
      let cash =
        if String.equal player.id cash_winner
        then
          Vdom.Node.span
            ~attrs:[ Vdom.Attr.classes [ "crown" ] ]
            [ Vdom.Node.text " 💸" ]
        else Vdom.Node.none
      in
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "player_results" ] ]
        [ Vdom.Node.h3
            ~attrs:[ Vdom.Attr.style name_style ]
            [ Vdom.Node.text player.id; crown; cash ]
        ; Vdom.Node.p
            ~attrs:[ Vdom.Attr.style name_style ]
            [ Vdom.Node.text ("Cash: " ^ Int.to_string player.cash) ]
        ])
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "player_results_div" ] ]
    children
;;

let body me winner winning_racer cash_winner players =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "results_page" ] ]
    [ Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "winner_results" ] ]
        [ Vdom.Node.h1
            ~attrs:[ Vdom.Attr.classes [ "winner" ] ]
            [ Vdom.Node.text "Race Winner:" ]
        ; Vdom.Node.img
            ~attrs:
              [ Vdom.Attr.src (Racer.to_img winning_racer)
              ; Vdom.Attr.classes [ "winner_img" ]
              ]
            ()
        ]
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "pot_winner" ] ]
        [ Vdom.Node.h2
            ~attrs:[ Vdom.Attr.classes [ "pot_winner_text" ] ]
            [ Vdom.Node.text ("Winner of Pot is: " ^ winner ^ "!") ]; Vdom.Node.h2
            ~attrs:[ Vdom.Attr.classes [ "pot_winner_text" ] ]
            [ Vdom.Node.text ("Cash winner is: " ^ cash_winner ^ "!") ]
        ]
    ; players_view ~me ~winner ~cash_winner players
    ]
;;
