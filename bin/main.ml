open Graphics
open Game
open Board
open Tetromino

let current_piece = ref (random_tetromino ())

let next_piece = ref (random_tetromino ())

let spawn_piece () = draw_tetromino !current_piece

let move_piece f =
  if check_valid (f !current_piece) board = false then ()
  else (
    draw_tetromino ~white_out:true !current_piece;
    current_piece := f !current_piece;
    draw_board board;
    draw_tetromino !current_piece)

let complete_move () =
  current_piece := !next_piece;
  next_piece := random_tetromino ();
  spawn_piece ()

let rec process_key_requests () =
  let event = wait_next_event [ Key_pressed ] in
  begin
    match event.key with
    (* quit *)
    | 'q' -> exit 0
    (* rotate piece left *)
    | 'm' -> move_piece rotate_left
    (* move piece left *)
    | 'a' -> move_piece move_left
    (* move piece right *)
    | 'd' -> move_piece move_right
    (* drop piece *)
    | 'n' ->
        draw_tetromino ~white_out:true !current_piece;
        drop !current_piece board;
        complete_move ()
    | _ -> ()
  end;
  process_key_requests ()

let main () =
  spawn_piece ();
  process_key_requests ()

(* Execute the game *)
let () =
  open_graph " 600x800";
  draw_outline ();
  main ()
