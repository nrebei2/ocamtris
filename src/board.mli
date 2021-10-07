(** Representation of a tetris board.

    This module represents the data stored and operations in the main
    play area, the board. *)

val rows : int

val columns : int

type board = char array array
(** [board] is a state of a board in time, represented as a 2D array of
    chars of size [rows]x[columns] The structure of board is the same as
    the printed structure in [draw_board]. For instance, [b.(0).(0)] is
    the top left of the board (row 0, column 0). A value of [' '] in the
    board means there is a no block at the cell at the position. A value
    of ['i'], ['o'], ..., or ['l'] in the board means there is a block
    filling a cell at the position, corresponding to [t.state] for a
    tetromino t.*)

(* [board] is the current state of the game board, initially created
   with no blocks filling any cells *)
val board : board

(* [board_pos (x, y)] is the bottom left position of board at position
   (x, y) *)
val board_pos : int * int

(* [tile_size] is the side length of each cell of the board in pixels *)
val tile_size : int

(* [draw_outline ()] draws the grid outline of the board to the GUI
   window *)
val draw_outline : unit -> unit

(* [draw_board b] draws the entire board [b] to the GUI window. *)
val draw_board : board -> unit

(* [draw_title ()] draws the Ocamtris title at the top of the screen. *)
val draw_title : unit -> unit

val get_lowest_possible :
  Tetromino.tetromino -> board -> Tetromino.tetromino
(** [get_lowest_possible t b] is [t'], the lowest possible valid place
    of [t] in [b] *)

val draw_tetromino :
  ?white_out:bool ->
  ?preview:bool ->
  Tetromino.tetromino ->
  unit
(** [draw_tetromino t] draws [t] to the GUI window. 
    white_out correponds to drawing over the piece with white
    preiew correponds to drawing the piece as a preview *)

val clear_board : board -> unit
(** [clear_board b] clears [b] by setting all elements to [' '] *)

val cleared_rows : board -> int
(** [cleared_rows b] is the number of complete lines in [b] *)

val clear_lines : board -> bool
(** [clear_lines b] checks if any rows of [b] are filled. If no rows are
    filled, then clear_lines is [false]. Otherwise, it edits [b] to
    [b'], where [b'] is [b] where all rows which are filled are cleared,
    and then returns [true]. *)

val check_valid : Tetromino.tetromino -> board -> bool
(** [check_valid t b] is [true] if there is no overlap with [b] and [t],
    and [t] is constrained in the bounds of the board. [false]
    otherwise. *)

val update_board : Tetromino.tetromino -> board -> unit
(** [update_board t b] edits [b] to contain [t]. *)

val drop : Tetromino.tetromino -> board -> unit
(** [drop t b] calls [update_board t' b], where [t'] is
    [get_lowest_possible t] *)
