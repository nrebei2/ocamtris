(* El-Tetris Algorithm implementation, with taking two steps into
   consideration (current and next piece).
   https://imake.ninja/el-tetris-an-improvement-on-pierre-dellacheries-algorithm/ *)

(** TODO for later if anyone feels like writing the documentation cause
    I dont want to *)

val landing_height : Tetromino.tetromino -> Board.board -> float

val row_transitions : Board.board -> float

val column_transitions : Board.board -> float

val holes : Board.board -> float

val get_well_sum : Board.board -> float

val get_best_possible_drop :
  Tetromino.tetromino ->
  Board.board ->
  Tetromino.tetromino ->
  string * Tetromino.tetromino
