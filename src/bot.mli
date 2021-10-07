val get_best_possible_drop : Board.board -> int -> unit
(** [get_best_possible_drop b depth] derives [t] by
    iterating thorough all possible drops of the current tetromino (and
    possibly looking ahead if [depth >= 2], as [depth] is the number of
    pieces the bot is looking at) and determining the best possible one
    through a score function, calculated with the help of
    https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/. *)
