(** Representation of a tetris board.

    This module represents the data stored in adventure files, including
    the rooms and exits. It handles loading of that data from JSON as
    well as querying the data. *)

val rows : int

val columns : int

type board = char array array
(** [board] is a state of a board in time, represented as a 2D array of
    chars of size [rows]x[columns] The structure of board is the same as
    the printed structure in [draw_board]. [b.(0).(0)] is the top left
    of the board (row 0, column 0) A value of [' '] in the board means
    there is a no block at the cell at the position. A value of ['i'],
    ['o'], ..., or ['l'] in the board means there is a block filling a
    cell at the position.*)

(* [board] is the current state of the game board, initially created
   with no blocks filling any cells *)
val board : board

(* [board_pos (x, y)] is the bottom left position of board at position
   (x, y)*)
val board_pos : int * int

(* [tile_size] is the side length of each cell of the board in pixels *)
val tile_size : int

(* [draw_outline ()] draws the grid outline of the board to the GUI
   window *)
val draw_outline : unit -> unit

(* [draw_board b] draws the entire board [b] to the GUI window. Should
   only be called after calling [clear_lines b]*)
val draw_board : board -> unit

val draw_tetromino : Tetromino.tetromino -> unit
(** [draw_tetromino t] draws [t] to the GUI window. *)

val clear_lines : board -> unit
(** [clear_lines b] edits [b] to [b'], where [b'] is [b] where all rows
    which are filled are cleared, and all rows above cleared row are
    lowered *)

val check_valid : Tetromino.tetromino -> board -> bool
(** [check_valid t b] is [true] if there is no overlap with [b] and [t],
    and [t] is constrained in the bounds of the board. [false] otherwise
    Should use this function after rotating, moving, and dropping piece
    to determine if move is valid*)

val update_board : Tetromino.tetromino -> board -> unit
(** [update_board t b] edits [b] to [b'], where [b'] is [b] with [t]
    part of [b]. Should only be called once the tetris piece is dropped
    or placed. *)

val drop : Tetromino.tetromino -> board -> unit
(** [drop t b] calls [update_board t' b], where [t'] is [t] at the
    lowest possible valid place in the board *)
