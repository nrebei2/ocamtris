(* Tetromino implementation*)

type tetromino = {
  state : char array array;
  col : int;
  row : int;
}

let i_piece =
  { state = [| [| 'i'; 'i'; 'i'; 'i' |] |]; col = 5; row = 0 }

let o_piece =
  { state = [| [| 'o'; 'o' |]; [| 'o'; 'o' |] |]; col = 5; row = 0 }

let t_Piece =
  {
    state = [| [| ' '; 't'; ' ' |]; [| 't'; 't'; 't' |] |];
    col = 5;
    row = 0;
  }

let s_piece =
  {
    state = [| [| ' '; 's'; 's' |]; [| 's'; 's'; ' ' |] |];
    col = 5;
    row = 0;
  }

let z_piece =
  {
    state = [| [| 'z'; 'z'; ' ' |]; [| ' '; 'z'; 'z' |] |];
    col = 5;
    row = 0;
  }

let j_Piece =
  {
    state = [| [| 'j'; ' '; ' ' |]; [| 'j'; 'j'; 'j' |] |];
    col = 5;
    row = 0;
  }

let l_piece =
  {
    state = [| [| ' '; ' '; 'l' |]; [| 'l'; 'l'; 'l' |] |];
    col = 5;
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

let rotate_left t =
  { state = rotate_array_left t.state; col = t.col; row = t.row }
(* TODO: Change col and row based on rotation, save it for the rotation
   specifics: https://tetris.fandom.com/wiki/SRS *)

let rotate_right t = t |> rotate_left |> rotate_left |> rotate_left

let move_left t = { state = t.state; col = t.col - 1; row = t.row }

let move_right t = { state = t.state; col = t.col + 1; row = t.row }

let move_down t = { state = t.state; col = t.col; row = t.row + 1 }

let random_tetromino () =
  let choices =
    [| i_piece; o_piece; t_Piece; s_piece; z_piece; j_Piece; l_piece |]
  in
  let rand = Random.int (Array.length choices) in
  Array.get choices rand