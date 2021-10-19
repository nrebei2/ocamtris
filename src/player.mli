type controls = {
  move_left : char;
  move_right : char;
  move_down : char;
  rotate_left : char;
  drop : char;
  hold : char;
}
(** controls respresenting the keybinds for a player in
    [player.controls] *)

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
(** mutable state of a player [player.bot] is [true] if the player is a
    bot, otherwise [false]. [player.board] is the players board.
    [player.bag_pos] is the current position of the player's
    [next_piece] in the bag. [player.current_piece] is the player's
    current piece. [player.next_piece] is the player's next piece.
    [player.held_piece] is the player's held piece. [None] if the player
    is currently not holding a tetrimino. [player.score] is the player's
    score. [player.controls] are the player's controls. *)

exception CantPlace of player
(** [CantPlace p] is called when [p] cant spawn a piece, i.e., another
    piece is covering its spawn. Thus is a game over for [p] *)

val generate_players : int -> int -> player list
(** [generate_players p_count b_count] returns [p], a list of players
    with [p_count] humans and [b_count] bots. *)

val reset_player : player -> player
(** [reset_player p] returns [p'], where [p'] is [p] with its board
    cleared and pieces reset *)

val process_human_players : player list -> unit
(** [process_human_players plist] processes each player [plist] and
    calls functions based on their respective controls *)

val process_bot_player : player -> unit
(** [process_bot_player p] processes [p]*)

(* [spawn_piece p] draws [p.current_piece] to [p.board] and the GUI *)
val spawn_piece : player -> unit

(* [move_piece_down p] clears [p.current_piece] from the board, moves
   and redraws [p.current_piece] down 1 cell *)
val move_piece_down : player -> unit