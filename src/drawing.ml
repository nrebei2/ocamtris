open Graphics
open Settings

(* [draw_title ()] draws the Ocamtris title at the top of the screen. *)

let draw_title () =
  set_color black;
  moveto
    (300 - (fst (text_size "Ocamtris") / 2))
    (780 - snd (text_size "Ocamtris"));
  draw_string "Ocamtris"

(* [draw_next_piece ()] draws the label for the next tetromino peice. *)

let draw_next_piece () =
  moveto
    (550 - (fst (text_size "Ocamtris") / 2))
    (550 - snd (text_size "Ocamtris"));
  draw_string "Next Piece"

(* [draw_instructions ()] draws the instructions for the game.*)
let draw_instructions () =
  moveto
    (500 - (fst (text_size "Ocamtris") / 2))
    (350 - snd (text_size "Ocamtris"));
  draw_string "Controls:";
  moveto
    (500 - (fst (text_size "Ocamtris") / 2))
    (325 - snd (text_size "Ocamtris"));
  draw_string "- Use \"A\" and \"D\" keys";
  moveto
    (500 - (fst (text_size "Ocamtris") / 2))
    (310 - snd (text_size "Ocamtris"));
  draw_string " to move left and right";
  moveto
    (500 - (fst (text_size "Ocamtris") / 2))
    (285 - snd (text_size "Ocamtris"));
  draw_string "- \"M\" to rotate left";
  moveto
    (500 - (fst (text_size "Ocamtris") / 2))
    (260 - snd (text_size "Ocamtris"));
  draw_string "- \"N\" to drop isntantly"

(* [draw_outline p] draws the grid outline of the board to the GUI
   window, with its lower left position at [p]*)
let draw_outline board_pos =
  set_color black;
  let tile_size = 30 in

  (* left column *)
  moveto (fst board_pos - 1) (snd board_pos - 1);
  lineto
    (fst board_pos - 1)
    (snd board_pos + (tile_size * fst settings.board_size));

  (* right column *)
  moveto
    (fst board_pos + (tile_size * snd settings.board_size))
    (snd board_pos);
  lineto
    (fst board_pos + (tile_size * snd settings.board_size))
    (snd board_pos + (tile_size * fst settings.board_size));

  (* bottom row *)
  moveto (fst board_pos) (snd board_pos - 1);
  lineto
    (fst board_pos + (tile_size * snd settings.board_size))
    (snd board_pos - 1);

  (* top row *)
  moveto (fst board_pos)
    (snd board_pos + (tile_size * fst settings.board_size));
  lineto
    (fst board_pos + (tile_size * snd settings.board_size))
    (snd board_pos + (tile_size * fst settings.board_size))
