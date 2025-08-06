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

  let create () =
    { red_bid = None
    ; red_ask = None
    ; yellow_bid = None
    ; yellow_ask = None
    ; blue_bid = None
    ; blue_ask = None
    ; green_bid = None
    ; green_ask = None
    }
  ;;
end

let red_orders (state : Orders.t Bonsai.t) inject =
  let%sub { Orders.red_bid; red_ask; _ } = state in
  let%arr red_bid and red_ask and inject in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "red" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/pink_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match red_bid with
                   | Some bid -> Int.to_string bid
                   | None -> "Bid")
              ; Vdom.Attr.classes [ "red_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_red_bid current_order))
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match red_ask with
                   | Some ask -> Int.to_string ask
                   | None -> "Ask")
              ; Vdom.Attr.classes [ "red_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_red_ask current_order))
              ]
            ()
        ]
    ]
;;

let yellow_orders state inject =
  let%sub { Orders.yellow_bid; yellow_ask; _ } = state in
  let%arr yellow_bid and yellow_ask and inject in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "yellow" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/updated_yellow_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match yellow_bid with
                   | Some bid -> Int.to_string bid
                   | None -> "Bid")
              ; Vdom.Attr.classes [ "yellow_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_yellow_bid current_order))
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match yellow_ask with
                   | Some ask -> Int.to_string ask
                   | None -> "Ask")
              ; Vdom.Attr.classes [ "yellow_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_yellow_ask current_order))
              ]
            ()
        ]
    ]
;;

let blue_orders state inject =
  let%sub { Orders.blue_bid; blue_ask; _ } = state in
  let%arr blue_bid and blue_ask and inject in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "blue" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/updated_blue_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match blue_bid with
                   | Some bid -> Int.to_string bid
                   | None -> "Bid")
              ; Vdom.Attr.classes [ "blue_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_blue_bid current_order))
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match blue_ask with
                   | Some ask -> Int.to_string ask
                   | None -> "Ask")
              ; Vdom.Attr.classes [ "blue_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_blue_ask current_order))
              ]
            ()
        ]
    ]
;;

let green_orders state inject =
  let%sub { Orders.green_bid; green_ask; _ } = state in
  let%arr green_bid and green_ask and inject in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.classes [ "green" ] ]
    [ Vdom.Node.img
        ~attrs:[ Vdom.Attr.src "../images/green_character.png" ]
        ()
    ; Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes [ "orders" ] ]
        [ Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match green_bid with
                   | Some bid -> Int.to_string bid
                   | None -> "Bid")
              ; Vdom.Attr.classes [ "green_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_green_bid current_order))
              ]
            ()
        ; Vdom.Node.input
            ~attrs:
              [ Vdom.Attr.placeholder
                  (match green_ask with
                   | Some ask -> Int.to_string ask
                   | None -> "Ask")
              ; Vdom.Attr.classes [ "green_order" ]
              ; Vdom.Attr.type_ "number"
              ; Vdom.Attr.on_input (fun _ current_order ->
                  inject (`Update_green_ask current_order))
              ]
            ()
        ]
    ]
;;

let submit_button inject =
  let%arr inject in
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.type_ "button"
      ; Vdom.Attr.classes [ "submit_orders" ]
      ; Vdom.Attr.on_click (fun _ -> inject `Submit_bids)
      ]
    [ Vdom.Node.text "Submit Orders" ]
;;

(* Bonsai component *)

let component state inject =
  let%arr red_orders = red_orders state inject
  and yellow_orders = yellow_orders state inject
  and blue_orders = blue_orders state inject
  and green_orders = green_orders state inject
  and submit_button = submit_button inject in
  Vdom.Node.div
    [ Vdom.Node.form
        ~attrs:[ Vdom.Attr.classes [ "exchange_page" ] ]
        [ red_orders
        ; yellow_orders
        ; blue_orders
        ; green_orders
        ; submit_button
        ]
    ]
;;

let updated_orders (me : Player.t Bonsai_web.Bonsai.t) (local_ graph) =
  let dispatcher = Rpc_effect.Rpc.dispatcher Rpcs.Client_message.rpc graph in
  let helper_func =
    let%arr me and dispatcher in
    me, dispatcher
  in
  let state, inject =
    Bonsai.state_machine_with_input
      ~default_model:(Orders.create ())
      ~apply_action:(fun ctx input model action ->
        match input with
        | Bonsai.Computation_status.Inactive -> model
        | Active ((me : Player.t), dispatcher) ->
          (match action with
           | `Update_red_ask order ->
             { model with red_ask = Int.of_string_opt order }
           | `Update_yellow_ask order ->
             { model with yellow_ask = Int.of_string_opt order }
           | `Update_green_ask order ->
             { model with green_ask = Int.of_string_opt order }
           | `Update_blue_ask order ->
             { model with blue_ask = Int.of_string_opt order }
           | `Update_red_bid order ->
             { model with red_bid = Int.of_string_opt order }
           | `Update_blue_bid order ->
             { model with blue_bid = Int.of_string_opt order }
           | `Update_yellow_bid order ->
             { model with yellow_bid = Int.of_string_opt order }
           | `Update_green_bid order ->
             { model with green_bid = Int.of_string_opt order }
           | `Submit_bids ->
             let red_bid_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Red
                   ; price = model.red_bid
                   ; order_type = Bid
                   }
             in
             let red_ask_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Red
                   ; price = model.red_ask
                   ; order_type = Ask
                   }
             in
             let blue_bid_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Blue
                   ; price = model.blue_bid
                   ; order_type = Bid
                   }
             in
             let blue_ask_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Blue
                   ; price = model.blue_ask
                   ; order_type = Ask
                   }
             in
             let yellow_bid_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Yellow
                   ; price = model.yellow_bid
                   ; order_type = Bid
                   }
             in
             let yellow_ask_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Yellow
                   ; price = model.yellow_ask
                   ; order_type = Ask
                   }
             in
             let green_bid_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Green
                   ; price = model.green_bid
                   ; order_type = Bid
                   }
             in
             let green_ask_query =
               Rpcs.Client_message.Query.Order_placed
                 Order.
                   { player_id = me.id
                   ; racer = Racer.Green
                   ; price = model.green_ask
                   ; order_type = Ask
                   }
             in
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher red_bid_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher red_ask_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher yellow_bid_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher yellow_ask_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher blue_bid_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher blue_ask_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher green_bid_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             Bonsai_web.Bonsai.Apply_action_context.schedule_event
               ctx
               (match%bind.Effect dispatcher green_ask_query with
                | Ok _ -> Effect.all_unit []
                | Error error ->
                  Effect.of_sync_fun eprint_s [%sexp (error : Error.t)]);
             model))
      helper_func
      graph
  in
  component state inject
;;
