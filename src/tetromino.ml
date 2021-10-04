(* Tetromino implementation*)

type tetromino = {
  piece : char;
  state : char array array;
  col : int;
  row : int;
}

let i_piece =
  { piece = 'I'; state = [| [| 'i'; 'i'; 'i'; 'i' |] |]; col = 5; row = 0 }

let o_piece =
  {
    piece = 'O';
    state = [| [| 'o'; 'o' |]; [| 'o'; 'o' |] |];
    col = 5;
    row = 0;
  }

let t_Piece =
  {
    piece = 'T';
    state = [| [| ' '; 't'; ' ' |]; [| 't'; 't'; 't' |] |];
    col = 5;
    row = 0;
  }

let s_piece =
  {
    piece = 'S';
    state = [| [| ' '; 's'; 's' |]; [| 's'; 's'; ' ' |] |];
    col = 5;
    row = 0;
  }

let z_piece =
  {
    piece = 'Z';
    state = [| [| 'z'; 'z'; ' ' |]; [| ' '; 'z'; 'z' |] |];
    col = 5;
    row = 0;
  }

let j_Piece =
  {
    piece = 'J';
    state = [| [| 'j'; ' '; ' ' |]; [| 'j'; 'j'; 'j' |] |];
    col = 5;
    row = 0;
  }

let l_piece =
  {
    piece = 'L';
    state = [| [| ' '; ' '; 'l' |]; [| 'l'; 'l'; 'l' |] |];
    col = 5;
    row = 0;
  }

let rec rotate_array_left ar =
  let rec rotate_list' acc = function
    | [] | [] :: _ -> acc
    | ar -> rotate_list' (List.map List.hd ar :: acc) (List.map List.tl ar)
  in
  ar |> Array.to_list |> List.map Array.to_list |> rotate_list' []
  |> Array.of_list |> Array.map Array.of_list

let rotate_left t =
  {
    piece = t.piece;
    state = rotate_array_left t.state;
    col = t.col;
    row = t.row;
  }
(* TODO: Change col and row based on rotation, save it for the rotation specifics:
  https://tetris.fandom.com/wiki/SRS *)

let rotate_right t = t |> rotate_left |> rotate_left |> rotate_left

let move_left t =
  { piece = t.piece; state = t.state; col = t.col - 1 ; row = t.row }

let move_right t =
  { piece = t.piece; state = t.state; col = t.col + 1 ; row = t.row }

let move_down t =
  { piece = t.piece; state = t.state; col = t.col; row = t.row + 1}

let random_tetromino () =
  let choices =
    [| i_piece; o_piece; t_Piece; s_piece; z_piece; j_Piece; l_piece |]
  in
  let rand = Random.int (Array.length choices) in
  Array.get choices rand

let draw_tetromino t = match t with
  | {piece; state; col; row} -> begin
    match piece with 
    | 'I' -> ()
    | 'O' -> ()
    | 'T' -> ()
    | 'S' -> ()
    | 'Z' -> ()
    | 'J' -> ()
    | 'L' -> ()
    | _ -> failwith "shouldnt happen ¯\\_(ツ)_/¯"
  end

