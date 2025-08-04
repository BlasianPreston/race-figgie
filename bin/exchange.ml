open! Core
open! Bonsai
open! Bonsai_web
open! Bonsai.Let_syntax
open! Virtual_dom.Vdom
open! Js_of_ocaml
open! Race_figgie

(* Prebuilt nodes *)

module Orders = struct
  type t =
    { red_bid : int option
    ; red_ask : int option
    ; yellow_bid : int option
    ; yellow_ask : int option
    ; blue_bid : int option
    ; blue_ask : int option
    ; green_bid : int option
    ; green_ask : int option
    }
  [@@deriving fields]
end

let red_orders =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "red" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/pink_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Bid"
              ; Vdom.Attr.classes [ "red_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Ask"
              ; Vdom.Attr.classes [ "red_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ]
    ]
;;

let yellow_orders : Vdom.Node.t =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "yellow" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/updated_yellow_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Bid"
              ; Vdom.Attr.classes [ "yellow_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Ask"
              ; Vdom.Attr.classes [ "yellow_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ]
    ]
;;

let blue_orders : Vdom.Node.t =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "blue" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/updated_blue_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Bid"
              ; Vdom.Attr.classes [ "blue_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Ask"
              ; Vdom.Attr.classes [ "blue_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ]
    ]
;;

let green_orders : Vdom.Node.t =
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "green" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/green_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Bid"
              ; Vdom.Attr.classes [ "green_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder "Ask"
              ; Vdom.Attr.classes [ "green_order" ]
              ; Vdom.Attr.type_ "number"
              ]
            ()
        ]
    ]
;;

(* Helpers *)

let get_input_value_by_class (class_name : string) (index : int)
  : string option
  =
  let open Js_of_ocaml in
  let open Js in
  let doc = Dom_html.document in
  let elements = doc##getElementsByClassName (string class_name) in
  match Js.Opt.to_option (elements##item index) with
  | None -> None
  | Some el ->
    (match Js.Opt.to_option (Dom_html.CoerceTo.input el) with
     | None -> None
     | Some input -> Some (to_string input##.value))
;;

let get_orders_for_racer ~player_id (racer : Racer.t) : Order.t list =
  let class_name = String.lowercase (Racer.to_string racer) ^ "_order" in
  let bid_str = get_input_value_by_class class_name 0 in
  let ask_str = get_input_value_by_class class_name 1 in
  let parse_price s = Option.bind s ~f:Int.of_string_opt in
  let bid_price = parse_price bid_str in
  let ask_price = parse_price ask_str in
  let make_order order_type price =
    Order.create ~player_id ~racer ~price:(Some price) ~order_type
  in
  List.filter_map
    [ Option.map bid_price ~f:(make_order Bid)
    ; Option.map ask_price ~f:(make_order Ask)
    ]
    ~f:Fn.id
;;

let submit_button ~player_id ~on_submit : Vdom.Node.t =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.type_ "button"
      ; Vdom.Attr.classes [ "submit_orders" ]
      ; Vdom.Attr.on_click (fun _evt ->
          let racers = [ Racer.Red; Yellow; Blue; Green ] in
          let all_orders =
            List.concat_map racers ~f:(get_orders_for_racer ~player_id)
          in
          on_submit all_orders)
      ]
    [ Vdom.Node.text "Submit Orders" ]
;;

(* Bonsai component *)

let component ~player_id =
  Vdom.Node.div
    [ Vdom.Node.form
        ~attrs:[ Vdom.Attr.classes [ "exchange_page" ] ]
        [ red_orders
        ; yellow_orders
        ; blue_orders
        ; green_orders
        ; submit_button ~player_id ~on_submit:(fun _ -> Ui_effect.Ignore)
        ]
    ]
;;

let serve_body ~player_id = component ~player_id
