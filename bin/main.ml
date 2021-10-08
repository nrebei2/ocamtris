open Graphics
open Game
open Board
open Tetromino
open Bot

let playable = ref true

let can_hold = ref true

let current_piece = ref (random_tetromino ())

let next_piece = ref (random_tetromino ())

let held_piece = ref None

(* TODO: The (12, 2) and (-4, 2) column row pairs are HARDCODED fix asap
   otherwise its gonna get worse *)
let rec spawn_piece () =
  draw_preview ();
  clear_draw_next_piece ();
  if check_valid !current_piece board then draw_tetromino !current_piece
  else game_over ()

and clear_piece t =
  draw_tetromino ~white_out:true t;
  draw_tetromino ~white_out:true (get_lowest_possible t board)

and clear_draw_next_piece () =
  draw_tetromino ~white_out:true
    { !current_piece with col = 12; row = 2 };
  draw_tetromino { !next_piece with col = 12; row = 2 }

and clear_draw_held_piece tmp =
  match !held_piece with
  | None -> ()
  | Some x ->
      draw_tetromino ~white_out:true
        { !current_piece with col = -4; row = 2 };
      draw_tetromino { x with col = -4; row = 2 };
      clear_piece tmp

and draw_preview () =
  draw_tetromino ~preview:true
    (get_lowest_possible !current_piece board)

and hold_piece () =
  match !held_piece with
  | None ->
      held_piece := Some (match_name_to_default !current_piece.name);
      complete_move false
  | Some x ->
      let tmp = !current_piece in
      held_piece := Some (match_name_to_default !current_piece.name);
      current_piece := x;
      clear_draw_held_piece tmp;
      spawn_piece ()

and move_piece f =
  (* TODO: Instead of just [()] try moving the piece left/right/up/down
     till it fits, probably at a maximum of two steps. There are most
     liekly specifics somewhere online. *)
  if check_valid (f !current_piece) board = false then ()
  else (
    clear_piece !current_piece;
    current_piece := f !current_piece;
    draw_preview ();
    draw_tetromino !current_piece)

and complete_move should_drop =
  draw_tetromino ~white_out:true !current_piece;
  draw_tetromino (get_lowest_possible !current_piece board);
  if should_drop then (
    drop !current_piece board;
    if clear_lines board then draw_board board;
    can_hold := true)
  else clear_draw_held_piece !current_piece;
  current_piece := !next_piece;
  next_piece := random_tetromino ();
  spawn_piece ()

(* TODO: Should call when we implement a time system (every n seconds
   where n increases over time) *)
and move_piece_down () =
  if check_valid (move_down !current_piece) board = false then
    complete_move true
  else (
    draw_tetromino ~white_out:true !current_piece;
    current_piece := move_down !current_piece;
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
   | 'n' -> complete_move true
   (* hold piece *)
   | 'h' ->
       if !can_hold then (
         hold_piece ();
         can_hold := false)
   | _ -> ());
  process_main_requests ()

(* TODO: Respresent leaderboard as json and use Yojson to read/write
   data *)
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

and process_bot () =
  clear_piece !current_piece;
  let process_get_best_request t =
    match get_best_possible_drop !current_piece board t with
    | "drop", x ->
        current_piece := x;
        draw_tetromino !current_piece;
        Unix.sleepf 1.;
        complete_move true
    | "hold", x ->
        hold_piece ();
        clear_piece !current_piece;
        current_piece := x;
        draw_tetromino !current_piece;
        draw_preview ();
        Unix.sleepf 1.;
        complete_move true
    | _ -> ()
  in
  (match !held_piece with
  | None -> process_get_best_request !next_piece
  | Some x -> process_get_best_request x);
  process_bot ()

and main_scene () =
  playable := true;
  clear_graph ();
  (* draw_title (); *)
  draw_outline ();
  spawn_piece ();
  move_piece_down ();
  process_main_requests ()
  (* process_bot () *)

(* Execute the game *)
let () =
  open_graph " 600x800";
  main_scene ()
