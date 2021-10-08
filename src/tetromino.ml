(* Tetromino implementation*)

type tetromino = {
  name : char;
  state : char array array;
  col : int;
  row : int;
}

let i_piece =
  {
    name = 'i';
    state = [| [| 'i'; 'i'; 'i'; 'i' |] |];
    col = 3;
    row = 0;
  }

let o_piece =
  {
    name = 'o';
    state = [| [| 'o'; 'o' |]; [| 'o'; 'o' |] |];
    col = 3;
    row = 0;
  }

let t_Piece =
  {
    name = 't';
    state = [| [| ' '; 't'; ' ' |]; [| 't'; 't'; 't' |] |];
    col = 3;
    row = 0;
  }

let s_piece =
  {
    name = 's';
    state = [| [| ' '; 's'; 's' |]; [| 's'; 's'; ' ' |] |];
    col = 3;
    row = 0;
  }

let z_piece =
  {
    name = 'z';
    state = [| [| 'z'; 'z'; ' ' |]; [| ' '; 'z'; 'z' |] |];
    col = 3;
    row = 0;
  }

let j_Piece =
  {
    name = 'j';
    state = [| [| 'j'; ' '; ' ' |]; [| 'j'; 'j'; 'j' |] |];
    col = 3;
    row = 0;
  }

let l_piece =
  {
    name = 'l';
    state = [| [| ' '; ' '; 'l' |]; [| 'l'; 'l'; 'l' |] |];
    col = 3;
    row = 0;
  }

let rec rotate_array_left ar =
  let rec rotate_list' acc = function
    | [] | [] :: _ -> acc
    | ar ->
        rotate_list' (List.map List.hd ar :: acc) (List.map List.tl ar)
  in
  ar |> Array.to_list |> List.map Array.to_list |> rotate_list' []
  |> Array.of_list |> Array.map Array.of_list

(* TODO: Change col and row based on rotation, save it till whoever
   wants to bother with the rotation specifics:
   https://tetris.fandom.com/wiki/SRS *)
let rotate_left t = { t with state = rotate_array_left t.state }

let rotate_right t = t |> rotate_left |> rotate_left |> rotate_left

let move_left  t = { t with col = t.col - 1 }

let move_right t = { t with col = t.col + 1 }

let move_down t = { t with row = t.row + 1 }


(* https://en.wikipedia.org/wiki/Fisher–Yates_shuffle *)
let shuffle x =
  let rec shuffle_aux n x =
    match n with
    | 0 -> ()
    | _ ->
        let rand = Random.int n in
        let tmp = x.(rand) in
        x.(rand) <- x.(n);
        x.(n) <- tmp;
        shuffle_aux (n - 1) x
  in

  shuffle_aux (Array.length x - 1) x;
  x

let bag = ref []

let rec random_tetromino () =
  match !bag with
  | [] ->
    Random.self_init ();
      bag :=
        [|
          i_piece; o_piece; t_Piece; s_piece; z_piece; j_Piece; l_piece;
        |]
        |> shuffle |> Array.to_list;
      random_tetromino ()
  | h :: t ->
      bag := t;
      h

let match_name_to_default c =
  match c with
  | 'i' -> i_piece
  | 'o' -> o_piece
  | 't' -> t_Piece
  | 's' -> s_piece
  | 'z' -> z_piece
  | 'j' -> j_Piece
  | 'l' -> l_piece
  | _ -> failwith "how"
