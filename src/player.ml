open Board
open Tetromino
open Graphics
open Bot
open Settings
open Yojson.Basic.Util

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

let controls_of_json json =
  let get p c =
    (json |> member "controls" |> to_list
    |> List.find (fun x -> x |> member "id" |> to_string = p)
    |> member c |> to_string).[0]
  in
  [
    {
      move_left = get "player" "left";
      move_right = get "player" "right";
      rotate_left = get "player" "rotate left";
      move_down = get "player" "move down";
      drop = get "player" "drop";
      hold = get "player" "hold";
    };
    {
      move_left = get "player1" "left";
      move_right = get "player1" "right";
      rotate_left = get "player1" "rotate left";
      move_down = get "player1" "move down";
      drop = get "player1" "drop";
      hold = get "player1" "hold";
    };
    {
      move_left = get "player2" "left";
      move_right = get "player2" "right";
      rotate_left = get "player2" "rotate left";
      move_down = get "player2" "move down";
      drop = get "player2" "drop";
      hold = get "player2" "hold";
    };
  ]

let settings_controls =
  let json_contents = Yojson.Basic.from_file "assets/controls.json" in
  try controls_of_json json_contents
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let controls =
  [
    {
      move_left = 'a';
      move_right = 'd';
      rotate_left = '1';
      move_down = 's';
      drop = '2';
      hold = '3';
    };
    {
      move_left = 'j';
      move_right = 'l';
      rotate_left = ']';
      move_down = 'k';
      drop = '[';
      hold = 'p';
    };
  ]

type garbage = {
  mutable drop : bool;
  mutable inc : int;
  mutable send : int;
}

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
  mutable cleared_4_rows : bool;
  garbage_info : garbage;
}

exception CantPlace of player

let player i =
  {
    bot = false;
    board =
      Array.make_matrix
        (fst settings.board_size)
        (snd settings.board_size)
        ' ';
    board_pos = (150 + (650 * i), 50);
    bag_pos = 1;
    current_piece = get_from_bag 0;
    next_piece = get_from_bag 1;
    held_piece = None;
    can_hold = true;
    score = 0;
    controls =
      (match settings.mode with
      | Alone | PvE -> List.hd settings_controls
      | _ -> List.nth settings_controls ((i + 1) mod 3));
    cleared_4_rows = false;
    garbage_info = { drop = false; inc = 0; send = 0 };
  }

let bot i = { (player i) with bot = true }

let generate_players p_c b_c =
  List.init p_c player @ List.init b_c (fun i -> bot (p_c + i))

let reset_player p =
  {
    p with
    board =
      Array.make_matrix
        (fst settings.board_size)
        (snd settings.board_size)
        ' ';
    bag_pos = 1;
    current_piece = get_from_bag 0;
    next_piece = get_from_bag 1;
    held_piece = None;
    can_hold = true;
  }

let draw_score (p : player) =
  let score_text = "Score: " ^ string_of_int p.score in
  let width, height = text_size score_text in
  set_color white;
  fill_rect (fst p.board_pos + 320) (snd p.board_pos + 650) width height;
  set_color black;
  moveto (fst p.board_pos + 320) (snd p.board_pos + 650);
  draw_string score_text

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
    (let col, row =
       if p.current_piece.name = 'i' then (10, 1) else (12, 2)
     in
     { p.current_piece with col; row })
    p.board_pos;
  draw_tetromino
    (let col, row =
       if p.next_piece.name = 'i' then (10, 1) else (12, 2)
     in
     { p.next_piece with col; row })
    p.board_pos

and clear_draw_held_piece tmp p =
  match p.held_piece with
  | None -> ()
  | Some x ->
      draw_tetromino ~white_out:true
        (let col, row =
           if p.current_piece.name = 'i' then (-6, 2) else (-4, 2)
         in
         { p.current_piece with col; row })
        p.board_pos;
      draw_tetromino
        (let col, row = if x.name = 'i' then (-6, 2) else (-4, 2) in
         { x with col; row })
        p.board_pos;
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

and send_garbage l p = add_garbage l p.board

and complete_move should_drop p =
  draw_tetromino ~white_out:true p.current_piece p.board_pos;
  draw_tetromino
    (get_lowest_possible p.current_piece p.board)
    p.board_pos;

  if should_drop then (
    drop p.current_piece p.board;
    let num_rows_cleared = cleared_rows p.board in
    let garbage_lines =
      match num_rows_cleared with
      | 1 ->
          p.score <- p.score + 100;
          0
      | 2 ->
          p.score <- p.score + 300;
          1
      | 3 ->
          p.score <- p.score + 500;
          2
      | 4 when p.cleared_4_rows ->
          p.score <- p.score + 1200;
          4
      | 4 when not p.cleared_4_rows ->
          p.score <- p.score + 800;
          4
      | _ -> 0
    in

    p.garbage_info.inc <-
      p.garbage_info.inc
      - (garbage_lines
        + if p.cleared_4_rows && garbage_lines > 0 then 1 else 0);
    if p.garbage_info.inc > 0 then p.garbage_info.drop <- true
    else (
      p.garbage_info.send <- p.garbage_info.send + -p.garbage_info.inc;
      p.garbage_info.inc <- 0);
    if num_rows_cleared < 4 then p.cleared_4_rows <- false
    else p.cleared_4_rows <- true;
    draw_score p;
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
    let key = read_key () in
    let process_key p =
      match key with
      | 'q' -> exit 0
      | _ when key = p.controls.rotate_left ->
          rotate_piece rotate_left p
      | _ when key = p.controls.move_right -> move_piece move_right p
      | _ when key = p.controls.move_left -> move_piece move_left p
      | _ when key = p.controls.move_down -> move_piece_down p
      | _ when key = p.controls.drop -> complete_move true p
      | _ when key = p.controls.hold ->
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
