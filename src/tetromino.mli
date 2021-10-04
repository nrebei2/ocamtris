(* Module for Tetromino's, the tetris pieces*)

(** [tetromino] is the tetromino of a tetromino placed on the board
    [piece] is the name
    [state] is how the piece looks like idk
    [col] is the column position on the board for the top left of state, 0-indexed
    [row] is the column position on the board for the top left of state, 0-indexed *)
type tetromino = {state : char array array; col : int; row : int}

(** default values for each type of pieces, and where they initially spawn on the board ([col], [row] in their record type) *)
val i_piece : tetromino
val o_piece : tetromino
val t_Piece : tetromino
val s_piece : tetromino
val z_piece : tetromino
val j_Piece : tetromino
val l_piece : tetromino

(** [rotate_right t] is [t'] where [t'] is [t] rotated left*)
val rotate_left : tetromino -> tetromino

(** [rotate_right t] is [t'] where [t'] is [t] rotated right*)
val rotate_right : tetromino -> tetromino

(** [move_left t] is [t'] where [t'] is [t] moved one cell to the left *)
val move_left : tetromino -> tetromino

(** [move_right t] is [t'] where [t'] is [t] moved one cell to the right *)
val move_right : tetromino -> tetromino

(** [move_down t] is [t'] where [t'] is [t] moved down one cell*)
val move_down : tetromino -> tetromino

(** [random_tetromino ()] is a random tetromino*)
val random_tetromino : unit -> tetromino

