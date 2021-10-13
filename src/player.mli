(* controls respresenting the keybinds for movements *)
type controls = {
  move_left : char;
  move_right : char;
  move_down : char;
  rotate_left : char;
  drop : char;
  hold : char;
}

(* mutable state of a player *)
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

(** [CantPlace p] is called when [p] cant spawn a piece, i.e., another piece is covering its spawn. Thus is a game over for [p] *)
exception CantPlace of player

(** [default_player ()] returns [p] where all fields are initialized as default for a player *)
val default_player : unit -> player

(** [default_player_2 ()] returns [p] where all fields are initialized as default for a player2 *)
val default_player_2 : unit -> player

(** [default_bot ()] returns [p] where all fields are initialized as default for a bot *)
val default_bot : unit -> player

(** [process_human_players plist] processes each player [plist] and calls functions based on their respective controls *)
val process_human_players : player list -> unit

(** [process_bot_player p] processes [p] *)
val process_bot_player : player -> unit

(* [spawn_piece p] draws [p.current_piece] to [p.board] and game *)
val spawn_piece : player -> unit

(* [move_piece_down p] clears [p.current_piece] from the board, edits [p.current_piece] to move down 1 cell, and redraws [p.current_piece] *)
val move_piece_down : player -> unit