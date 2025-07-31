open Core

let advance_race (state : Game_state.State.t) =
  Game_state.State.update_positions state
;;

let check_winner (state : Game_state.State.t) =
  let race_positions = state.race_positions in
  List.fold race_positions ~init:state ~f:(fun state positions ->
    match positions with
    | racer, position, _ ->
      if position >= 500
      then Game_state.State.set_winner state (Some racer)
      else state)
;;
