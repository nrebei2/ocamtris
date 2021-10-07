(* Representation of a tetromino.

   This module represents the data stored and operations for a general
   tetromino. *)

type tetromino = {
  name : char;
  state : char array array;
  col : int;
  row : int;
}
(** [tetromino] is the tetromino of a tetromino placed on the board
    [name] is the name of the piece [state] is how the piece looks like
    idk [col] is the column position relative to the board for the top
    left of state, 0-indexed [row] is the row position relative to the
    board for the top left of state, 0-indexed. For example, for a
    tetromino with [col = 0] and [row = 0] corresponds to the piece on
    the top left of the board *)

val i_piece : tetromino
(** default values for each type of pieces, and where they initially
    spawn on the board ([col], [row] in their record type) *)

val o_piece : tetromino

val t_Piece : tetromino

val s_piece : tetromino

val z_piece : tetromino

val j_Piece : tetromino

val l_piece : tetromino

val rotate_left : tetromino -> tetromino
(** [rotate_right t] is [t'] where [t'] is [t] rotated left *)

val rotate_right : tetromino -> tetromino
(** [rotate_right t] is [t'] where [t'] is [t] rotated right*)

val move_left : tetromino -> tetromino
(** [move_left t] is [t'] where [t'] is [t] moved one cell to the left *)

val move_right : tetromino -> tetromino
(** [move_right t] is [t'] where [t'] is [t] moved one cell to the right *)

val move_down : tetromino -> tetromino
(** [move_down t] is [t'] where [t'] is [t] moved down one cell*)

val random_tetromino : unit -> tetromino
(** [random_tetromino ()] is a random tetromino*)

val match_name_to_default : char -> tetromino
(** [match_name_to_default c] is one of the above default values based
    on [c] Example: [match_name_to_default 'i'] is [i_piece]*)
