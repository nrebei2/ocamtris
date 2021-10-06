(* Board implementation*)

open Graphics
open Tetromino

let rows = 20

let columns = 10

type board = char array array

let board = Array.make_matrix columns rows ' '

let board_pos = (100, 50)

let tile_size = 30

let title_font_size = 50

let draw_title () =
  set_font ("-*-fixed-medium-r-semicondensed--" ^ (string_of_int title_font_size) ^ "-*-*-*-*-*-iso8859-1");
  moveto (300 - ((fst (text_size "Ocamtris")) / 2)) (780 - (snd (text_size "Ocamtris")));
  draw_string "Ocamtris"

let draw_board () =
  draw_title ();
  for i = 0 to columns do
    moveto (fst board_pos + (tile_size * i)) (snd board_pos);
    lineto (fst board_pos + (tile_size * i)) (snd board_pos + (tile_size * rows))
  done;
  for j = 0 to rows do
    moveto (fst board_pos) (snd board_pos + (tile_size * j));
    lineto
      (fst board_pos + (tile_size * columns))
      (snd board_pos + (tile_size * j))
  done

(** Gets height of board (highest row a tetis piece is in)*)
let get_height b =
  let heights = Array.make columns 0 in
  Array.fold_right
    (fun x acc ->
      Array.iteri
        (fun i x ->
          if x <> ' ' then Array.set heights i (Array.get heights i + 1) else ())
        x)
    b ();
  List.fold_left max 0 (Array.to_list heights)

(** update score here with [get_height b - Array.length uncleared_rows] lines cleared ; *)
let clear_lines b =
  let uncleared_rows =
    Array.concat
      (List.rev
         (Array.fold_left
            (fun acc x ->
              if Array.fold_left (fun acc x -> acc && x == ' ') true x then acc
              else [| x |] :: acc)
            [] b))
  in
  let fill_rows =
    Array.make (columns - Array.length uncleared_rows) (Array.make rows ' ')
  in
  Array.append fill_rows uncleared_rows

let check_valid t b =
  let checks =
    Array.make (Array.length t.state)
      (Array.make (Array.length t.state.(0)) false)
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
  with _ -> false
(* out of bounds*)

let update_board t b =
  Array.iteri
    (fun r rowv -> Array.iteri (fun c v -> b.(r + t.row).(c + t.col) <- v) rowv)
    t.state;
    b

let rec drop t b =
  let new_t = Tetromino.move_down t in
  if check_valid new_t b then drop new_t b else update_board t b

