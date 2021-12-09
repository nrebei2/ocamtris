open Graphics
open Tetromino
open Theme
open Settings

type board = char array array

(* TODO: color from any position, and remove repeated code in
   draw_2D_array*)
let color_cell color r c board_pos =
  let tile_size = 30 in
  set_color color;
  fill_rect
    ((c * tile_size) + fst board_pos)
    (((fst settings.board_size - r - 1) * tile_size) + snd board_pos)
    (tile_size - 1) (tile_size - 1)

let draw_2D_aray
    ?draw_white:(b = false)
    ?white_out:(b2 = false)
    ?preview:(b3 = false)
    ar
    row_start
    column_start
    board_pos =
  for r = 0 to Array.length ar - 1 do
    for c = 0 to Array.length ar.(0) - 1 do
      let r' = r + row_start in
      let c' = c + column_start in
      let theme c = c |> colors_of_pallete settings.theme in
      if b3 then
        match ar.(r).(c) with
        | ' ' -> ()
        | _ -> color_cell (theme Preview) r' c' board_pos
      else if b2 then
        match ar.(r).(c) with
        | ' ' -> ()
        | _ -> color_cell (theme Background) r' c' board_pos
      else
        match ar.(r).(c) with
        | 'i' -> color_cell (theme I) r' c' board_pos
        | 'o' -> color_cell (theme O) r' c' board_pos
        | 't' -> color_cell (theme T) r' c' board_pos
        | 's' -> color_cell (theme S) r' c' board_pos
        | 'z' -> color_cell (theme Z) r' c' board_pos
        | 'j' -> color_cell (theme J) r' c' board_pos
        | 'l' -> color_cell (theme L) r' c' board_pos
        | 'g' -> color_cell black r' c' board_pos
        | ' ' ->
            if b then
              color_cell
                (Background |> colors_of_pallete settings.theme)
                r' c' board_pos
            else ()
        | _ -> failwith "shouldnt happen ¯\\_(ツ)_/¯"
    done
  done

let draw_board b = draw_2D_aray ~draw_white:true b 0 0

let draw_tetromino ?white_out:(b2 = false) ?preview:(b3 = false) t =
  match t with
  | { name; state; col = c; row = r } ->
      draw_2D_aray ~white_out:b2 ~preview:b3 state r c

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
      acc && List.mem x [ 'i'; 'o'; 't'; 's'; 'z'; 'j'; 'l'; 'g' ])
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
  if cleared_rows b = 0 then false
  else
    let rec make n =
      match n with
      | 0 -> []
      | _ -> Array.make (Array.length b.(0)) ' ' :: make (n - 1)
    in
    let new_board =
      make (fst settings.board_size - List.length uncleared_rows)
      @ uncleared_rows
    in
    new_board |> copy_to_2D_array b;
    true

let add_garbage n b =
  let rec create_garbage_line n b =
    match n with
    | 0 -> []
    | _ ->
        let l = Array.make (Array.length b.(0)) 'g' in
        l.(Random.int (Array.length b.(0))) <- ' ';
        l :: create_garbage_line (n - 1) b
  in
  let new_board =
    Array.to_list (Array.sub b n (Array.length b - n))
    @ create_garbage_line n b
  in
  new_board |> copy_to_2D_array b

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
