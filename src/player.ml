open Board
open Tetromino
open Graphics
open Bot

(* TODO: make controls modular (player can choose them at the start of
   the game) *)

type controls = {
  move_left : char;
  move_right : char;
  move_down : char;
  rotate_left : char;
  drop : char;
  hold : char;
}

let controls =
  [
    {
      move_left = 'a';
      move_right = 'd';
      rotate_left = 'm';
      move_down = 's';
      drop = 'n';
      hold = 'h';
    };
    {
      move_left = 'j';
      move_right = 'l';
      rotate_left = 'p';
      move_down = 'k';
      drop = 'o';
      hold = 'i';
    };
  ]

type player = {
  bot : bool;
  board : board;
  board_pos : int * int;
  mutable bag_pos : int;
  mutable current_piece : Tetromino.tetromino;
  mutable next_piece : Tetromino.tetromino;
  mutable held_piece : Tetromino.tetromino option;
  mutable can_hold : bool;
  mutable score : int;
  mutable controls : controls;
}

exception CantPlace of player

let player i =
  {
    bot = false;
    board = Array.make_matrix rows columns ' ';
    board_pos = (150 + (650 * i), 50);
    bag_pos = 1;
    current_piece = get_from_bag 0;
    next_piece = get_from_bag 1;
    held_piece = None;
    can_hold = true;
    score = 0;
    controls = List.hd controls;
  }

let bot i = { (player i) with bot = true }

let generate_players p_c b_c =
  List.init p_c player @ List.init b_c (fun i -> bot (p_c + i))

let reset_player p =
  {
    p with
    board = Array.make_matrix rows columns ' ';
    bag_pos = 1;
    current_piece = get_from_bag 0;
    next_piece = get_from_bag 1;
    held_piece = None;
    can_hold = true;
  }

let process_wall_kicks f p =
  let cur = f p.current_piece in
  match cur.name with
  | 'o' -> if check_valid cur p.board then Some cur else None
  | _ -> (
      let off_d =
        match cur.name with 'i' -> i_offset_data | _ -> offset_data
      in
      let kick_translations =
        Array.map2
          (fun x y -> (fst x - fst y, snd x - snd y))
          off_d.(p.current_piece.rot)
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
          match 0 with
          | _ when check 0 -> Some (translate 0)
          | _ -> None
        end
      | _ -> begin
          match 0 with
          | _ when check 0 -> Some (translate 0)
          | _ when check 1 -> Some (translate 1)
          | _ when check 2 -> Some (translate 2)
          | _ when check 3 -> Some (translate 3)
          | _ when check 4 -> Some (translate 4)
          | _ -> None
        end)

let rec spawn_piece p =
  draw_preview p;
  clear_draw_next_piece p;
  if check_valid p.current_piece p.board then
    draw_tetromino p.current_piece p.board_pos
  else raise (CantPlace p)

and clear_piece t p =
  draw_tetromino ~white_out:true t p.board_pos;
  draw_tetromino ~white_out:true
    (get_lowest_possible t p.board)
    p.board_pos

(* TODO: printing the held I piece doesnt look good, maybe dont try hard
   coding the col and row and try something else *)

and clear_draw_next_piece p =
  draw_tetromino ~white_out:true
    { p.current_piece with col = 12; row = 2 }
    p.board_pos;
  draw_tetromino { p.next_piece with col = 12; row = 2 } p.board_pos

and clear_draw_held_piece tmp p =
  match p.held_piece with
  | None -> ()
  | Some x ->
      draw_tetromino ~white_out:true
        { p.current_piece with col = -5; row = 2 }
        p.board_pos;
      draw_tetromino { x with col = -5; row = 2 } p.board_pos;
      clear_piece tmp p

and draw_preview p =
  draw_tetromino ~preview:true
    (get_lowest_possible p.current_piece p.board)
    p.board_pos

and hold_piece p =
  match p.held_piece with
  | None ->
      p.held_piece <- Some (match_name_to_default p.current_piece.name);
      complete_move false p
  | Some x ->
      let tmp = p.current_piece in
      p.held_piece <- Some (match_name_to_default p.current_piece.name);
      p.current_piece <- x;
      clear_draw_held_piece tmp p;
      spawn_piece p

and place t p =
  clear_piece p.current_piece p;
  p.current_piece <- t;
  draw_preview p;
  draw_tetromino p.current_piece p.board_pos

and move_piece f p =
  if check_valid (f p.current_piece) p.board then
    place (f p.current_piece) p

and rotate_piece f p =
  match process_wall_kicks f p with Some x -> place x p | None -> ()

and complete_move should_drop p =
  draw_tetromino ~white_out:true p.current_piece p.board_pos;
  draw_tetromino
    (get_lowest_possible p.current_piece p.board)
    p.board_pos;
  if should_drop then (
    drop p.current_piece p.board;
    (* TODO: call [cleared_rows p.board] match the number of cleared
       rows with score increase p.score p.score <- p.score +
       score_you_want_to_increase_it_with call a function which print
       the players score on the GUI *)
    if clear_lines p.board then draw_board p.board p.board_pos;
    p.can_hold <- true)
  else clear_draw_held_piece p.current_piece p;
  p.current_piece <- p.next_piece;
  p.bag_pos <- p.bag_pos + 1;
  p.next_piece <- get_from_bag p.bag_pos;
  spawn_piece p

(* TODO: add https://tetris.wiki/Lock_delay *)
and move_piece_down p =
  if check_valid (move_down p.current_piece) p.board = false then
    complete_move true p
  else (
    draw_tetromino ~white_out:true p.current_piece p.board_pos;
    p.current_piece <- move_down p.current_piece;
    draw_tetromino p.current_piece p.board_pos)

let process_human_players p_lst =
  if key_pressed () then
    let event = wait_next_event [ Key_pressed ] in
    let process_key p =
      match event.key with
      | 'q' -> exit 0
      | _ when event.key = p.controls.rotate_left ->
          rotate_piece rotate_left p
      | _ when event.key = p.controls.move_right ->
          move_piece move_right p
      | _ when event.key = p.controls.move_left ->
          move_piece move_left p
      | _ when event.key = p.controls.move_down -> move_piece_down p
      | _ when event.key = p.controls.drop -> complete_move true p
      | _ when event.key = p.controls.hold ->
          if p.can_hold then (
            hold_piece p;
            p.can_hold <- false)
      | _ -> ()
    in
    List.iter process_key p_lst

let process_bot_player p =
  clear_piece p.current_piece p;
  let process_get_best_request t =
    match get_best_possible_drop p.current_piece p.board t with
    | "drop", x ->
        p.current_piece <- x;
        draw_tetromino p.current_piece p.board_pos;
        complete_move true p
    | "hold", x ->
        hold_piece p;
        clear_piece p.current_piece p;
        p.current_piece <- x;
        draw_tetromino p.current_piece p.board_pos;
        draw_preview p;
        complete_move true p
    | _ -> ()
  in
  match p.held_piece with
  | None -> process_get_best_request p.next_piece
  | Some x -> process_get_best_request x
