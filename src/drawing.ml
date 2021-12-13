open Graphics
open Settings
open Player

(** [draw_title ()] draws the Ocamtris title at the top of the screen. *)
let draw_title () =
  set_color black;
  moveto
    (300 - (fst (text_size "Ocamtris") / 2))
    (780 - snd (text_size "Ocamtris"));
  draw_string "Ocamtris"

(** [draw_next_piece ()] draws the label for the next tetromino peice. *)
let draw_next_piece board_pos =
  moveto
    (fst board_pos + 400 - (fst (text_size "Ocamtris") / 2))
    (snd board_pos + 500 - snd (text_size "Ocamtris"));
  draw_string "Next Piece"

  let draw_instructions_helper p font_siz str =
    moveto
      (fst p.board_pos + 350 - (fst (text_size "Ocamtris") / 2))
      (font_siz - snd (text_size "Ocamtris"));
    draw_string str
    
   (* [draw_instructions ()] draws the instructions for the game.*)
   let draw_instructions p =
    draw_instructions_helper p 350 "Controls:";
    draw_instructions_helper p 325
      (Printf.sprintf "- Use \"%c\" and \"%c\" keys" p.controls.move_left
         p.controls.move_right);
    draw_instructions_helper p 310 " to move left and right";
    draw_instructions_helper p 285
      (Printf.sprintf "- \"%c\" to rotate left" p.controls.rotate_left);
    draw_instructions_helper p 260
      (Printf.sprintf "- \"%c\" to drop isntantly" p.controls.drop);
    draw_instructions_helper p 235
      (Printf.sprintf "- \"%c\" to soft drop" p.controls.move_down);
    draw_instructions_helper p 210
      (Printf.sprintf "- \"%c\" to hold" p.controls.hold)

(** [draw_outline p] draws the grid outline of the board to the GUI
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
