(* El-Tetris Algorithm implementation, with taking two steps into
   consideration (either drop or hold and drop).
   https://imake.ninja/el-tetris-an-improvement-on-pierre-dellacheries-algorithm/ *)

(* Decriptions of features are copied from above link *)

val landing_height : Tetromino.tetromino -> Board.board -> float
(** [landing_height t b] is height where once [t] is dropped on [b] (=
    the height of the column + (the height of the piece / 2)) *)

val row_transitions : Board.board -> float
(** [row_transitions b] is the total number of row transitions in [b]. A
    row transition occurs when an empty cell is adjacent to a filled
    cell on the same row and vice versa.*)

val column_transitions : Board.board -> float
(** [column_transitions b] is the total number of column transitions in
    [b]. A column transition occurs when an empty cell is adjacent to a
    filled cell on the same column and vice versa*)

val holes : Board.board -> float
(** [holes b] is the total number of holes in [b]. A hole is an empty
    cell that has at least one filled cell above it in the same column.*)

val get_well_sum : Board.board -> float
(** [get_well_sum b] is the well sum total of [b]. A well is a
    succession of empty cells such that the top cell has their left
    cells and right cells filled. *)

val get_best_possible_drop :
  Tetromino.tetromino ->
  Board.board ->
  Tetromino.tetromino ->
  string * Tetromino.tetromino
(** [get_best_possible_drop t b t'] is [(v, t_best)]. [t] is the current
    piece while [t'] is the next piece. The function generates all
    possible positions and rotations for [t] and [t'], and determines
    the best piece [t_best] which drop results in the maximum possible
    score, determined by a score function. If [t_best] is from [t], [v]
    is ["drop"], otherwise ["hold"]. *)
