open Graphics
open Game
open Board
open Tetromino
open Bot

let past_time = ref 0.

let drop_timer = ref 0.

let bot_timer = ref 0.

type controls = {
  move_left : char;
  move_right : char;
  rotate_left : char;
  drop : char;
  hold : char;
}

let offset_data =
  [|
    [| (0, 0); (0, 0); (0, 0); (0, 0); (0, 0) |];
    [| (0, 0); (-1, 0); (-1, -1); (0, 2); (-1, 2) |];
    [| (0, 0); (0, 0); (0, 0); (0, 0); (0, 0) |];
    [| (0, 0); (1, 0); (1, -1); (0, 2); (1, 2) |];
  |]

let i_offset_data =
  [|
    [| (0, 0); (-1, 0); (2, 0); (-1, 0); (2, 0) |];
    [| (0, -1); (0, 1); (0, 1); (0, -1); (0, 2) |];
    [| (-1, -1); (1, 1); (-2, 1); (1, 0); (-2, 0) |];
    [| (-1, 0); (0, 0); (0, 0); (0, 1); (0, -2) |];
  |]

let o_offset_data =
  [| [| (0, 0) |]; [| (0, 0) |]; [| (0, 0) |]; [| (0, 0) |] |]

(* TODO: make controls modular (player can choose them at the start of
   the game) *)

let player1_controls =
  {
    move_left = 'a';
    move_right = 'd';
    rotate_left = 'm';
    drop = 'n';
    hold = 'h';
  }

let player2_controls =
  {
    move_left = 'j';
    move_right = 'l';
    rotate_left = 'p';
    drop = 'o';
    hold = 'i';
  }

let playable = ref true

let rec spawn_piece p =
  draw_preview p;
  clear_draw_next_piece p;
  if check_valid !(p.current_piece) p.board then
    draw_tetromino !(p.current_piece) p
  else game_over ()

and clear_piece t p =
  draw_tetromino ~white_out:true t p;
  draw_tetromino ~white_out:true (get_lowest_possible t p.board) p

(* TODO: printing the held I piece doesnt look good, maybe dont try hard coding the col and row and try something else *)

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
        { !(p.current_piece) with col = -5; row = 2 }
        p;
      draw_tetromino { x with col = -5; row = 2 } p;
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

and process_wall_kicks f p =
  let cur = f !(p.current_piece) in
  let off_d =
    match cur.name with
    | 'i' -> i_offset_data
    | 'o' -> o_offset_data
    | _ -> offset_data
  in
  let kick_translations =
    Array.map2
      (fun x y -> (fst x - fst y, snd x - snd y))
      off_d.(!(p.current_piece).rot)
      off_d.(cur.rot)
  in

  let translate x =
    {
      cur with
      row = cur.row + snd kick_translations.(x);
      col = cur.col + fst kick_translations.(x);
    }
  in
  let check x = check_valid (translate x) p.board in
  match cur.name with
  | 'o' -> begin
      match 0 with _ when check 0 -> Some (translate 0) | _ -> None
    end
  | _ -> begin
      match 0 with
      | _ when check 0 -> Some (translate 0)
      | _ when check 1 -> Some (translate 1)
      | _ when check 2 -> Some (translate 2)
      | _ when check 3 -> Some (translate 3)
      | _ when check 4 -> Some (translate 4)
      | _ -> None
    end

and place x p =
  clear_piece !(p.current_piece) p;
  p.current_piece := x;
  draw_preview p;
  draw_tetromino !(p.current_piece) p

and move_piece f p =
  if check_valid (f !(p.current_piece)) p.board then
    place (f !(p.current_piece)) p

and rotate_piece f p =
  match process_wall_kicks f p with Some x -> place x p | None -> ()

and complete_move should_drop p =
  draw_tetromino ~white_out:true !(p.current_piece) p;
  draw_tetromino (get_lowest_possible !(p.current_piece) p.board) p;
  if should_drop then (
    drop !(p.current_piece) p.board;
    if clear_lines p.board then draw_board p;
    p.can_hold := true)
  else clear_draw_held_piece !(p.current_piece) p;
  p.current_piece := !(p.next_piece);
  p.bag_pos := !(p.bag_pos) + 1;
  p.next_piece := get_from_bag !(p.bag_pos);
  spawn_piece p

(* TODO: add https://tetris.wiki/Lock_delay *)
and move_piece_down p =
  if check_valid (move_down !(p.current_piece)) p.board = false then
    complete_move true p
  else (
    draw_tetromino ~white_out:true !(p.current_piece) p;
    p.current_piece := move_down !(p.current_piece);
    draw_tetromino !(p.current_piece) p)

and process_main_requests () =
  (if key_pressed () then
   let event = wait_next_event [ Key_pressed ] in
   if !playable then (
     let process_key p k =
       match event.key with
       | 'q' -> exit 0
       | _ when event.key = k.rotate_left -> rotate_piece rotate_left p
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
     process_key player2 player2_controls));
  drop_timer := !drop_timer +. Sys.time () -. !past_time;
  if !drop_timer > 0.1 then (
    move_piece_down player1;
    move_piece_down player2;
    drop_timer := 0.)

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
  bot_timer := !bot_timer +. Sys.time () -. !past_time;
  if !bot_timer > 0.1 then (
    clear_piece !(p.current_piece) p;
    let process_get_best_request t =
      match get_best_possible_drop !(p.current_piece) p.board t with
      | "drop", x ->
          p.current_piece := x;
          draw_tetromino !(p.current_piece) p;
          complete_move true p
      | "hold", x ->
          hold_piece p;
          clear_piece !(p.current_piece) p;
          p.current_piece := x;
          draw_tetromino !(p.current_piece) p;
          draw_preview p;
          complete_move true p
      | _ -> ()
    in
    begin
      match !(p.held_piece) with
      | None -> process_get_best_request !(p.next_piece)
      | Some x -> process_get_best_request x
    end;
    bot_timer := 0.)

and process_players () =
  process_main_requests ();
  process_bot player2;
  past_time := Sys.time ();
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

(* Execute the game *)
let () =
  open_graph " 1350x800";
  main_scene ()
