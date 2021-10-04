(* Board implementation*)

open Graphics
open Tetromino

let rows = 20

let columns = 10

type board = char array array

let board = Array.make_matrix rows columns ' '

let board_pos = (100, 50)

let tile_size = 30

let draw_outline () =
  set_color black;
  for i = 0 to columns do
    moveto (fst board_pos + (tile_size * i)) (snd board_pos);
    lineto
      (fst board_pos + (tile_size * i))
      (snd board_pos + (tile_size * rows))
  done;
  for j = 0 to rows do
    moveto (fst board_pos) (snd board_pos + (tile_size * j));
    lineto
      (fst board_pos + (tile_size * columns))
      (snd board_pos + (tile_size * j))
  done

let color_cell color r c =
  set_color color;
  fill_rect
    ((c * tile_size) + fst board_pos)
    (((rows - r - 1) * tile_size) + snd board_pos)
    tile_size tile_size

let draw_2D_aray ?draw_white:(b = false) ar row_start column_start =
  for r = 0 to Array.length ar - 1 do
    for c = 0 to Array.length ar.(0) - 1 do
      let r' = r + row_start in
      let c' = c + column_start in
      match ar.(r).(c) with
      | 'i' -> color_cell cyan r' c'
      | 'o' -> color_cell yellow r' c'
      | 't' -> color_cell magenta r' c'
      | 's' -> color_cell green r' c'
      | 'z' -> color_cell red r' c'
      | 'j' -> color_cell blue r' c'
      | 'l' -> color_cell (rgb 255 165 0) r' c'
      | ' ' -> if b then color_cell white r' c' else ()
      | _ -> failwith "shouldnt happen ¯\\_(ツ)_/¯"
    done
  done;
  draw_outline ()

let draw_board b = draw_2D_aray ~draw_white:true b 0 0

let draw_tetromino t =
  match t with { state; col = c; row = r } -> draw_2D_aray state r c

(** Gets height of board (highest row a tetis piece is in)*)
let get_height b =
  let heights = Array.make columns 0 in
  Array.fold_right
    (fun x acc ->
      Array.iteri
        (fun i x ->
          if x <> ' ' then Array.set heights i (Array.get heights i + 1)
          else ())
        x)
    b ();
  List.fold_left max 0 (Array.to_list heights)

let copy_2D_array_to b1 b2 =
  for r = 0 to Array.length b1 - 1 do
    for c = 0 to Array.length b1.(0) - 1 do
      b1.(r).(c) <- b2.(r).(c)
    done
  done

(** Optimization needed to be done, maybe if needed update score here
    with [get_height b - Array.length uncleared_rows] lines cleared ; *)
let clear_lines b =
  let uncleared_rows =
    Array.concat
      (List.rev
         (Array.fold_left
            (fun acc x ->
              if
                Array.fold_left
                  (fun acc x ->
                    acc
                    && List.mem x [ 'i'; 'o'; 't'; 's'; 'z'; 'j'; 'l' ])
                  true x
              then 
                (* cleared row, use mutable data type (ref) to increase lines cleared*)
                acc
              else (
                [| x |] :: acc))
            [] b))
  in
  let fill_rows =
    Array.make_matrix (rows - Array.length uncleared_rows) columns ' '
  in
  uncleared_rows |> Array.append fill_rows |> copy_2D_array_to b

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
  with _ -> false
(* out of bounds*)

let update_board t b =
  Array.iteri
    (fun r rowv ->
      Array.iteri (fun c v -> b.(r + t.row).(c + t.col) <- v) rowv)
    t.state

let rec drop t b =
  let new_t = Tetromino.move_down t in
  if check_valid new_t b then drop new_t b else update_board t b
