open Graphics
open Game
open Board
open Tetromino

let rec main () =
  let event = wait_next_event [ Key_pressed ] in
  if event.key == 'q' then exit 0 else main ()

(* Execute the game *)
let () =
  open_graph " 600x800";
  set_color black;
  draw_outline ();
  drop j_Piece board;
  drop (i_piece |> rotate_left) board;
  clear_lines board;
  drop
    {
      state =
        [| [| 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; 'l' |] |];
      col = 0;
      row = 0;
    }
    board;
  clear_lines board;
  draw_board board;
  main ()
