open Graphics
open Game
open Board
open Tetromino
open Bot

type controls = {
  move_left : char;
  move_right : char;
  rotate_left : char;
  drop : char;
  hold : char;
}

let player1_controls =
  {
    move_left = 'a';
    move_right = 'd';
    rotate_left = 'c';
    drop = 'v';
    hold = 'f';
  }

let player2_controls =
  {
    move_left = 'j';
    move_right = 'l';
    rotate_left = 'b';
    drop = 'n';
    hold = 'h';
  }

let playable = ref true

(* TODO: The (12, 2) and (-4, 2) column row pairs are HARDCODED fix asap
   otherwise its gonna get worse *)
let rec spawn_piece p =
  draw_preview p;
  clear_draw_next_piece p;
  if check_valid !(p.current_piece) p.board then
    draw_tetromino !(p.current_piece) p
  else game_over ()

and clear_piece t p =
  draw_tetromino ~white_out:true t p;
  draw_tetromino ~white_out:true (get_lowest_possible t p.board) p

and clear_draw_next_piece p =
  draw_tetromino ~white_out:true
    { !(p.current_piece) with col = 12; row = 2 }
    p;
  draw_tetromino { !(p.next_piece) with col = 12; row = 2 } p

and clear_draw_held_piece tmp p =
  match !(p.held_piece) with
  | None -> ()
  | Some x ->
      draw_tetromino ~white_out:true
        { !(p.current_piece) with col = -4; row = 2 }
        p;
      draw_tetromino { x with col = -4; row = 2 } p;
      clear_piece tmp p

and draw_preview p =
  draw_tetromino ~preview:true
    (get_lowest_possible !(p.current_piece) p.board)
    p

and hold_piece p =
  match !(p.held_piece) with
  | None ->
      p.held_piece :=
        Some (match_name_to_default !(p.current_piece).name);
      complete_move false p
  | Some x ->
      let tmp = !(p.current_piece) in
      p.held_piece :=
        Some (match_name_to_default !(p.current_piece).name);
      p.current_piece := x;
      clear_draw_held_piece tmp p;
      spawn_piece p

and move_piece f p =
  (* TODO: Instead of just [()] try moving the piece left/right/up/down
     till it fits, probably at a maximum of two steps. There are most
     liekly specifics somewhere online. *)
  if check_valid (f !(p.current_piece)) p.board = false then ()
  else (
    clear_piece !(p.current_piece) p;
    p.current_piece := f !(p.current_piece);
    draw_preview p;
    draw_tetromino !(p.current_piece) p)

and complete_move should_drop p =
  draw_tetromino ~white_out:true !(p.current_piece) p;
  draw_tetromino (get_lowest_possible !(p.current_piece) p.board) p;
  if should_drop then (
    drop !(p.current_piece) p.board;
    if clear_lines p.board then draw_board p;
    p.can_hold := true)
  else clear_draw_held_piece !(p.current_piece) p;
  p.current_piece := !(p.next_piece);
  p.next_piece := random_tetromino ();
  spawn_piece p

(* TODO: Should call when we implement a time system (every n seconds
   where n increases over time) *)
and move_piece_down p =
  if check_valid (move_down !(p.current_piece)) p.board = false then
    complete_move true p
  else (
    draw_tetromino ~white_out:true !(p.current_piece) p;
    p.current_piece := move_down !(p.current_piece);
    draw_tetromino !(p.current_piece) p)

and process_main_requests () =
  let event = wait_next_event [ Key_pressed ] in
  if !playable then (
    let process_key p k =
      match event.key with
      | 'q' -> exit 0
      | _ when event.key = k.rotate_left -> move_piece rotate_left p
      | _ when event.key = k.move_right -> move_piece move_right p
      | _ when event.key = k.move_left -> move_piece move_left p
      | _ when event.key = k.drop -> complete_move true p
      | _ when event.key = k.hold ->
          if !(p.can_hold) then (
            hold_piece p;
            p.can_hold := false)
      | _ -> ()
    in
    process_key player1 player1_controls;
    process_key player2 player2_controls)

(* TODO: Respresent leaderboard as json and use Yojson to read/write
   data *)
and display_leaderboard () =
  moveto 100 100;
  draw_string "leaderboard here"

and game_over () =
  playable := false;
  clear_graph ();
  clear_board player1.board;
  clear_board player2.board;
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

and process_bot p =
  clear_piece !(p.current_piece) p;
  let process_get_best_request t =
    match get_best_possible_drop !(p.current_piece) p.board t with
    | "drop", x ->
        p.current_piece := x;
        draw_tetromino !(p.current_piece) p;
        (* Unix.sleepf 1.; *)
        complete_move true p
    | "hold", x ->
        hold_piece p;
        clear_piece !(p.current_piece) p;
        p.current_piece := x;
        draw_tetromino !(p.current_piece) p;
        draw_preview p;
        (* Unix.sleepf 1.; *)
        complete_move true p
    | _ -> ()
  in
  match !(p.held_piece) with
  | None -> process_get_best_request !(p.next_piece)
  | Some x -> process_get_best_request x


(* TODO: make function works independantly of each other (wait_next_event is causing the issue) *)
and process_players () =
  process_main_requests ();
  process_bot player2;
  process_players ()

and main_scene () =
  playable := true;
  clear_graph ();
  (* draw_title (); *)
  draw_outline player1;
  draw_outline player2;
  spawn_piece player1;
  spawn_piece player2;
  process_players ()
(* move_piece_down (); *)

(* Execute the game *)
let () =
  open_graph " 1350x800";
  main_scene ()
