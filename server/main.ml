open! Core
open! Async_kernel
open! Bonsai_web
open Async_js
open Bonsai_chat_open_source_common
open Composition_infix

module Room_state = struct
  type t =
    { messages : Message.t list
    ; current_room : Room.t option
    }
  [@@deriving fields ~getters]
end

let run () =
  Async_js.init ();
  let%bind conn = Rpc.Connection.client_exn () in
  let rooms_list_var = Bonsai.Expert.Var.create [] in
  let room_state_var =
    Bonsai.Expert.Var.create { Room_state.messages = []; current_room = None }
  in
  let change_room = change_room ~conn ~room_state_var in
  let refresh_rooms = refresh_rooms ~conn ~rooms_list_var in
  let send_message = send_message ~conn in
  let () =
    Bonsai_web.Start.start
      (App.component
         ~room_list:(Bonsai.Expert.Var.value rooms_list_var)
         ~current_room:
           (Bonsai.map
              ~f:Room_state.current_room
              (Bonsai.Expert.Var.value room_state_var))
         ~messages:
           (Bonsai.map ~f:Room_state.messages (Bonsai.Expert.Var.value room_state_var))
         ~refresh_rooms
         ~change_room
         ~send_message)
  in
  don't_wait_for (run_refresh_rooms ~conn ~rooms_list_var);
  don't_wait_for (process_message_stream ~conn ~room_state_var);
  return ()
;;

let () = don't_wait_for (run ())