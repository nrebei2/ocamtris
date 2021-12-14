open Board
open Tetromino

let landing_height t b =
  let drop = get_lowest_possible t b in
  let landing_height =
    Array.length b - (drop.row + Array.length drop.state)
  in
  float landing_height +. (float (Array.length drop.state) /. 2.)

let row_transitions b =
  let count = ref 0 in
  for i = 0 to Array.length b.(0) - 1 do
    for j = 0 to Array.length b - 1 do
      if i = 0 then ()
      else if
        (b.(j).(i) = ' ' && b.(j).(i - 1) <> ' ')
        || (b.(j).(i) <> ' ' && b.(j).(i - 1) = ' ')
      then count := !count + 1
    done
  done;
  float !count

let column_transitions b =
  let count = ref 0 in
  for i = 0 to Array.length b.(0) - 1 do
    for j = 0 to Array.length b - 1 do
      if j = 0 then ()
      else if
        (b.(j).(i) = ' ' && b.(j - 1).(i) <> ' ')
        || (b.(j).(i) <> ' ' && b.(j - 1).(i) = ' ')
      then count := !count + 1
    done
  done;
  float !count

let get_heights b =
  let heights = Array.make (Array.length b.(0)) 0 in
  let checks = Array.make (Array.length b.(0)) true in
  Array.fold_left
    (fun acc x ->
      Array.iteri
        (fun i x ->
          if checks.(i) then
            if x == ' ' then heights.(i) <- 1 + heights.(i)
            else checks.(i) <- false)
        x)
    () b;
  heights |> Array.map (fun x -> Array.length b - x)

let get_well_sum b =
  let well_sums = ref 0 in
  let heights = get_heights b in
  for i = 0 to Array.length b - 1 do
    for j = 0 to Array.length b.(0) - 1 do
      if i < Array.length b - heights.(j) then
        if
          (j = 0 && b.(i).(j) = ' ' && b.(i).(j + 1) <> ' ')
          || j = Array.length b.(0) - 1
             && b.(i).(j) = ' '
             && b.(i).(j - 1) <> ' '
          || j > 0
             && j < Array.length b.(0) - 1
             && b.(i).(j) = ' '
             && b.(i).(j + 1) <> ' '
             && b.(i).(j - 1) <> ' '
        then well_sums := !well_sums + Array.length b - heights.(j) - i
    done
  done;
  float !well_sums

let aggregate_height b =
  b |> get_heights |> Array.to_list |> List.fold_left ( + ) 0 |> float

let holes b =
  let heights_no_air = Array.make (Array.length b.(0)) 0 in
  Array.fold_left
    (fun acc x ->
      Array.iteri
        (fun i x ->
          if x <> ' ' then heights_no_air.(i) <- 1 + heights_no_air.(i))
        x)
    () b;
  b |> get_heights
  |> Array.mapi (fun i h -> h - heights_no_air.(i))
  |> Array.fold_left ( + ) 0 |> float

let bumpiness b =
  let r = ref 0 in
  let heights = b |> get_heights in
  Array.iteri
    (fun i h ->
      if i = 0 then () else r := !r + abs (h - heights.(i - 1)))
    heights;
  float !r

let copy_2D ar =
  Array.fold_left (fun acc x -> Array.copy x :: acc) [] ar
  |> List.rev |> Array.of_list

let score t b =
  let new_board = copy_2D b in
  let cleared = float (Board.cleared_rows new_board) in
  drop t new_board;
  let _ = clear_lines new_board in
  (-4.500158825082766 *. landing_height t b)
  +. (3.4181268101392694 *. cleared)
  +. (-3.2178882868487753 *. row_transitions new_board)
  +. (-9.348695305445199 *. column_transitions new_board)
  +. (-7.899265427351652 *. holes new_board)
  +. (-3.3855972247263626 *. get_well_sum new_board)

let compare_score b d1 d2 =
  let score_d1 = score d1 b in
  let score_d2 = score d2 b in
  if score_d1 >= score_d2 then d1 else d2


let rec positions f t b =
  match check_valid (t |> f) b with 
  | true -> (t |> f) :: positions f (t |> f) b
  | false -> []

(* *)
let rec generate_all_possible_drops t b =
  List.fold_left
    (fun acc x ->
      x :: (x |> rotate_left)
      :: (x |> rotate_left |> rotate_left)
      :: (x |> rotate_right) :: acc)
    [] [ t ]
  |> List.fold_left
       (fun acc x ->
         (positions move_left x b) @ [x] @ (positions move_right x b) @ acc)
       []

let rec get_best_possible_drop t b next_piece =
  let drops = generate_all_possible_drops t b in
  let next_drops = generate_all_possible_drops next_piece b in
  let rec get_best drops b =
    match drops with
    | [] -> failwith "x"
    | [ x ] -> x
    | h :: t -> compare_score b h (get_best t b)
  in
  let best_cur = get_best drops b in
  let best_next = get_best next_drops b in
  if score best_cur b > score best_next b then ("drop", best_cur)
  else ("hold", best_next)
