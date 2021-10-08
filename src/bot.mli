val get_best_possible_drop :
  Tetromino.tetromino -> Board.board -> Tetromino.tetromino
(** [get_best_possible_drop t b] is [t']. [t'] is derived by iterating
    through all rotations and positions of [t], then determining the
    best possible drop through a score function, calculated with the
    help of
    https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/. *)
