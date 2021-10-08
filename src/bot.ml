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

let aggregate_height b =
  b |> get_heights |> Array.to_list |> List.fold_left ( + ) 0

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
  |> Array.fold_left ( + ) 0

let bumpiness b =
  let r = ref 0 in
  let heights = b |> get_heights in
  Array.iteri
    (fun i h ->
      if i = 0 then () else r := !r + abs (h - heights.(i - 1)))
    heights;
  !r

let score b =
  (-0.510066 *. float (aggregate_height b))
  +. (0.760666 *. float (Board.cleared_rows b))
  +. (-0.35663 *. float (holes b))
  +. (-0.184483 *. float (bumpiness b))

open Board
open Tetromino

let copy_2D ar =
  Array.fold_left (fun acc x -> Array.copy x :: acc) [] ar
  |> List.rev |> Array.of_list

let compare_score b d1 d2 =
  let b1 = copy_2D b in
  drop d1 b1;
  let score_d1 = score b1 in
  let b2 = copy_2D b in
  drop d2 b2;
  let score_d2 = score b2 in
  if score_d1 >= score_d2 then d1 else d2

(* *)
let rec generate_all_possible_drops t =
  List.fold_left
    (fun acc x ->
      x :: (x |> rotate_left)
      :: (x |> rotate_left |> rotate_left)
      :: (x |> rotate_right) :: acc)
    [] [ t ]
  |> List.fold_left
       (fun acc x ->
         let l, r =
           (-x.col, columns - Array.length x.state.(0) - x.col)
         in
         let rec generate_pieces max cur =
           if cur > max then []
           else
             { x with col = x.col + cur }
             :: generate_pieces max (cur + 1)
         in
         generate_pieces r l @ acc)
       []

(* TODO: Add depth by looking at next piece and maybe held piece *)
let rec get_best_possible_drop t b =
  let drops = generate_all_possible_drops t in
  let rec get_best drops b =
    match drops with
    | [] -> failwith "x"
    | [ x ] -> x
    | h :: t -> compare_score b h (get_best t b)
  in
  get_best drops b

let test =
  List.fold_left (fun acc x -> (x + 1) :: (x + 2) :: acc) [] [ 1 ]
