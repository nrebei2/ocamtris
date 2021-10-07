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

(* TODO for wayyyyy later *)
let rec get_best_possible_drop b depth = ()