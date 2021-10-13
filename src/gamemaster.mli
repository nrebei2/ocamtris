type timers = {
  mutable update : float;
  mutable drop_timer : float;
  mutable bot_timer : float;
}

type game = {
  mutable over : bool;
  mutable player1 : Player.player;
  mutable player2 : Player.player option;
  mutable gravity : float;
  difficulty : float;
  mutable timers : timers;
}

val default_timer : unit -> timers

val run : game -> unit 