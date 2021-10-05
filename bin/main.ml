open Graphics
open Game
open Board
open Tetromino

let playable = ref true

let current_piece = ref (random_tetromino ())

let next_piece = ref (random_tetromino ())

(* Scenes *)
let rec spawn_piece () =
  draw_preview ();
  draw_tetromino ~white_out:true
    { !current_piece with col = 12; row = 2 };
  draw_tetromino { !next_piece with col = 12; row = 2 };
  if check_valid !current_piece board then draw_tetromino !current_piece
  else game_over ()

and draw_preview () =
  draw_tetromino ~preview:true (get_lowest_possible !current_piece board);

and move_piece f =
  if check_valid (f !current_piece) board = false then ()
  else (
    draw_tetromino ~draw_white:true !current_piece;
    current_piece := f !current_piece;
    draw_board board;
    draw_tetromino !current_piece);
    draw_preview ();

and complete_move () =
  draw_tetromino ~draw_white:true !current_piece;
  drop !current_piece board;
  current_piece := !next_piece;
  next_piece := random_tetromino ();
  spawn_piece ()

(* Should call when we implement a time system (every n seconds where n
   increases over time) *)
and move_piece_down () =
  if check_valid (move_down !current_piece) board = false then
    complete_move ()
  else (
    draw_tetromino ~draw_white:true !current_piece;
    current_piece := move_down !current_piece;
    draw_board board;
    draw_tetromino !current_piece)

and process_main_requests () =
  let event = wait_next_event [ Key_pressed ] in
  (if !playable then
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
   | 'n' -> complete_move ()
   | _ -> ());
   process_main_requests ()

and display_leaderboard () =
  moveto 100 100;
  draw_string "leaderboard here"

and game_over () =
  playable := false;
  clear_graph ();
  clear_board board;
  display_leaderboard ();
  moveto 350 100;
  draw_string "press r to retyry press q to quit";  
  process_game_over_requests ()

and process_game_over_requests () = 
  let event2 = wait_next_event [ Key_pressed ] in
    if not !playable then
      match event2.key with
      | 'q' -> exit 0
      | 'r' -> main_scene ()
      | _ ->
          ();
          process_game_over_requests ()

and main_scene () =
  playable := true;
  clear_graph ();
  draw_outline ();
  spawn_piece ();
  process_main_requests ()

(* Execute the game *)
let () =
  open_graph " 600x800";
  main_scene ()
