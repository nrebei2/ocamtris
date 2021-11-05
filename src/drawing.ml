open Graphics
open Settings
open Player

(* [draw_title ()] draws the Ocamtris title at the top of the screen. *)

let draw_title () =
  set_color black;
  moveto
    (300 - (fst (text_size "Ocamtris") / 2))
    (780 - snd (text_size "Ocamtris"));
  draw_string "Ocamtris"

(* [draw_next_piece ()] draws the label for the next tetromino peice. *)

let draw_next_piece board_pos =
  moveto
    ((fst board_pos + 400) - (fst (text_size "Ocamtris") / 2))
    ((snd board_pos + 500) - snd (text_size "Ocamtris"));
  draw_string "Next Piece"

(* [draw_instructions ()] draws the instructions for the game.*)
let draw_instructions p =
  moveto
    ((fst p.board_pos + 350) - (fst (text_size "Ocamtris") / 2))
    (350 - snd (text_size "Ocamtris"));
  draw_string "Controls:";
  moveto
    ((fst p.board_pos + 350) - (fst (text_size "Ocamtris") / 2))
    (325 - snd (text_size "Ocamtris"));
  draw_string (Printf.sprintf "- Use \"%c\" and \"%c\" keys" p.controls.move_left p.controls.move_right);
  moveto
    ((fst p.board_pos + 350) - (fst (text_size "Ocamtris") / 2))
    (310 - snd (text_size "Ocamtris"));
  draw_string " to move left and right";
  moveto
    ((fst p.board_pos + 350) - (fst (text_size "Ocamtris") / 2))
    (285 - snd (text_size "Ocamtris"));
  draw_string (Printf.sprintf "- \"%c\" to rotate left" p.controls.rotate_left);
  moveto
    ((fst p.board_pos + 350) - (fst (text_size "Ocamtris") / 2))
    (260 - snd (text_size "Ocamtris"));
  draw_string (Printf.sprintf "- \"%c\" to drop isntantly" p.controls.drop);
  moveto
    ((fst p.board_pos + 350) - (fst (text_size "Ocamtris") / 2))
    (235 - snd (text_size "Ocamtris"));
  draw_string (Printf.sprintf "- \"%c\" to soft drop" p.controls.move_down);
  moveto
    ((fst p.board_pos + 350) - (fst (text_size "Ocamtris") / 2))
    (210 - snd (text_size "Ocamtris"));
  draw_string (Printf.sprintf "- \"%c\" to hold" p.controls.hold)

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
