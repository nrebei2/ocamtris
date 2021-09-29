(* module for board *)

val rows : int
val columns : int

(* bottom left position of board *)
val board_pos : int * int

(* size of each square *)
val tile_size : int

(* draw the board *)
val draw_board : unit -> unit