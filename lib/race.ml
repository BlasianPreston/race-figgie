open Core

let advance_race (state : Game_state.t) = Game_state.update_positions state

let check_winner (state : Game_state.t) =
  let race_positions = state.race_positions in
  List.fold race_positions ~init:state ~f:(fun state positions ->
    match positions with
    | racer, position, _ ->
      if position >= 500
      then Game_state.set_winner state (Some racer)
      else state)
;;
