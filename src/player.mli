type controls = {
  move_left : char;
  move_right : char;
  move_down : char;
  rotate_left : char;
  drop : char;
  hold : char;
}

type player = {
  bot : bool;
  board : Board.board;
  board_pos : int * int;
  mutable bag_pos : int;
  mutable current_piece : Tetromino.tetromino;
  mutable next_piece : Tetromino.tetromino;
  mutable held_piece : Tetromino.tetromino option;
  mutable can_hold : bool;
  mutable score : int;
  mutable controls : controls;
}

exception CantPlace of player

val default_player : unit -> player

val default_player_2 : unit -> player

val default_bot : unit -> player

val process_human_players : player list -> unit

val process_bot_player : player -> unit

val spawn_piece : player -> unit

val move_piece_down : player -> unit