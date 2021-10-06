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

let move_left t = { t with col = t.col - 1 }

let move_right t = { t with col = t.col + 1 }

let move_down t = { t with row = t.row + 1 }

(** TODO: Not actually random, look into seeds or something *)
let random_tetromino () =
  let choices =
    [| i_piece; o_piece; t_Piece; s_piece; z_piece; j_Piece; l_piece |]
  in
  let rand = Random.int (Array.length choices) in
  Array.get choices rand

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
