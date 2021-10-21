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
    tetromino [t].*)

(* [tile_size] is the side length of each cell of the board in pixels *)
val tile_size : int

(* [draw_outline p] draws the grid outline of the board to the GUI
   window, with its lower left position at [p]*)
val draw_outline : int * int -> unit

(* [draw_board b p] draws the entire board [b] to the GUI window, with
   its lower left position at [p] *)
val draw_board : board -> int * int -> unit

(* [draw_title ()] draws the Ocamtris title at the top of the screen. *)
val draw_title : unit -> unit

(* [draw_next_piece ()] draws the label for the next tetromino peice. *)
val draw_next_piece : unit -> unit

(* [draw_instructions ()] draws the instructions for the game.*)
val draw_instructions : unit -> unit

val draw_tetromino :
  ?white_out:bool ->
  ?preview:bool ->
  Tetromino.tetromino ->
  int * int ->
  unit
(** [draw_tetromino t p] draws [t] to the board with lower left position
    at [p]. [white_out] correponds to drawing over the piece with white.
    [preview] correponds to drawing the piece as a preview (gray)*)

val clear_board : board -> unit
(** [clear_board b] clears [b] by setting all elements to [' '] *)

val cleared_rows : board -> int
(** [cleared_rows b] is the number of complete lines in [b]. A complete
    line in [b] is a row such that no elements are [' '] *)

val clear_lines : board -> bool
(** [clear_lines b] checks if any rows of [b] are filled. If no rows are
    filled, then clear_lines is [false]. Otherwise, it edits [b] to
    [b'], where [b'] is [b] where all rows which are filled are cleared,
    and then returns [true]. *)

val check_valid : Tetromino.tetromino -> board -> bool
(** [check_valid t b] is [true] if there is no overlap with the existing
    tetrominos in [b] and tetromino [t], and [t] is constrained in the
    bounds of the board. [false] otherwise. *)

val update_board : Tetromino.tetromino -> board -> unit
(** [update_board t b] edits [b] to contain [t]. Requires
    [check_valid t b] to be [true]. *)

val get_lowest_possible :
  Tetromino.tetromino -> board -> Tetromino.tetromino
(** [get_lowest_possible t b] is [t'], the lowest possible valid place
    of [t] in [b] *)

val drop : Tetromino.tetromino -> board -> unit
(** [drop t b] calls [update_board (get_lowest_possible t) b] *)
