(* Board implementation*)

open Graphics
open Tetromino

let rows = 20

let columns = 10

type board = char array array

type player = {
  board : board;
  board_pos : int * int;
  bag_pos : int ref;
  current_piece : Tetromino.tetromino ref;
  next_piece : Tetromino.tetromino ref;
  held_piece : Tetromino.tetromino option ref;
  can_hold : bool ref;
}

let player1 =
  {
    board = Array.make_matrix rows columns ' ';
    board_pos = (150, 50);
    bag_pos = ref 1;
    current_piece = ref (get_from_bag 0);
    next_piece = ref (get_from_bag 1);
    held_piece = ref None;
    can_hold = ref true
  }

let player2 =
  {
    board = Array.make_matrix rows columns ' ';
    board_pos = (850, 50);
    bag_pos = ref 1;
    current_piece = ref (get_from_bag 0);
    next_piece = ref (get_from_bag 1);
    held_piece = ref None;
    can_hold = ref true
  }

let tile_size = 30

let title_font_size = 50

let draw_title () =
  set_font
    ("-*-fixed-medium-r-semicondensed--"
    ^ string_of_int title_font_size
    ^ "-*-*-*-*-*-iso8859-1");
  moveto
    (300 - (fst (text_size "Ocamtris") / 2))
    (780 - snd (text_size "Ocamtris"));
  draw_string "Ocamtris"

let draw_outline p =
  set_color black;
  for i = 0 to columns do
    moveto (fst p.board_pos + (tile_size * i)) (snd p.board_pos);
    lineto
      (fst p.board_pos + (tile_size * i))
      (snd p.board_pos + (tile_size * rows))
  done;
  for j = 0 to rows do
    moveto (fst p.board_pos) (snd p.board_pos + (tile_size * j));
    lineto
      (fst p.board_pos + (tile_size * columns))
      (snd p.board_pos + (tile_size * j))
  done

(* TODO: change to pasting a picture instead *)
let color_cell color r c p =
  set_color color;
  fill_rect
    ((c * tile_size) + fst p.board_pos)
    (((rows - r - 1) * tile_size) + snd p.board_pos)
    tile_size tile_size

let draw_2D_aray
    ?draw_white:(b = false)
    ?white_out:(b2 = false)
    ?preview:(b3 = false)
    ar
    row_start
    column_start
    p =
  for r = 0 to Array.length ar - 1 do
    for c = 0 to Array.length ar.(0) - 1 do
      let r' = r + row_start in
      let c' = c + column_start in
      if b3 then
        match ar.(r).(c) with
        | ' ' -> ()
        | _ -> color_cell (rgb 200 200 200) r' c' p
      else if b2 then
        match ar.(r).(c) with
        | ' ' -> ()
        | _ -> color_cell white r' c' p
      else
        match ar.(r).(c) with
        | 'i' -> color_cell cyan r' c' p
        | 'o' -> color_cell yellow r' c' p
        | 't' -> color_cell magenta r' c' p
        | 's' -> color_cell green r' c' p
        | 'z' -> color_cell red r' c' p
        | 'j' -> color_cell blue r' c' p
        | 'l' -> color_cell (rgb 255 165 0) r' c' p
        | ' ' -> if b then color_cell white r' c' p else ()
        | _ -> failwith "shouldnt happen ¯\\_(ツ)_/¯"
    done
  done;
  draw_outline p

let draw_board p = draw_2D_aray ~draw_white:true p.board 0 0 p

let draw_tetromino ?white_out:(b2 = false) ?preview:(b3 = false) t p =
  match t with
  | { name; state; col = c; row = r } ->
      draw_2D_aray ~white_out:b2 ~preview:b3 state r c p

let copy_to_2D_array b1 b2 =
  for r = 0 to Array.length b1 - 1 do
    for c = 0 to Array.length b1.(0) - 1 do
      b1.(r).(c) <- (List.nth b2 r).(c)
    done
  done

let clear_board b =
  for r = 0 to Array.length b - 1 do
    for c = 0 to Array.length b.(0) - 1 do
      b.(r).(c) <- ' '
    done
  done

let filled =
  Array.fold_left
    (fun acc x ->
      acc && List.mem x [ 'i'; 'o'; 't'; 's'; 'z'; 'j'; 'l' ])
    true

let cleared_rows b =
  List.length
    (List.rev
       (Array.fold_left
          (fun acc x -> if filled x then Array.copy x :: acc else acc)
          [] b))

let clear_lines b =
  let uncleared_rows =
    List.rev
      (Array.fold_left
         (fun acc x ->
           if
             filled x
             || Array.fold_left (fun acc x -> acc && x == ' ') true x
           then acc
           else Array.copy x :: acc)
         [] b)
  in
  (* TODO: [cleared_rows b] is the number of cleared rows, use mutable
     data type (ref) to increase lines cleared/score *)
  if cleared_rows b = 0 then false
  else
    let rec make n =
      match n with
      | 0 -> []
      | _ -> Array.make (Array.length b.(0)) ' ' :: make (n - 1)
    in
    let new_board =
      make (rows - List.length uncleared_rows) @ uncleared_rows
    in
    new_board |> copy_to_2D_array b;
    true

let check_valid t b =
  let checks =
    Array.make_matrix (Array.length t.state)
      (Array.length t.state.(0))
      false
  in
  try
    Array.iteri
      (fun r v ->
        Array.iteri
          (fun c v ->
            if v == ' ' || b.(r + t.row).(c + t.col) == ' ' then
              checks.(r).(c) <- true
            else ())
          v)
      t.state;
    List.fold_left ( && ) true
      (checks |> Array.to_list |> List.map Array.to_list |> List.concat)
    (* out of bounds*)
  with _ -> false

let update_board t b =
  Array.iteri
    (fun r rowv ->
      Array.iteri
        (fun c v -> if v <> ' ' then b.(r + t.row).(c + t.col) <- v)
        rowv)
    t.state

let rec get_lowest_possible t b =
  let new_t = Tetromino.move_down t in
  if check_valid new_t b then get_lowest_possible new_t b else t

let rec drop t b = update_board (get_lowest_possible t b) b
