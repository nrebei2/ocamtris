(* mutable state of timer
  [timers.update] is the time since last frame
  [timers.drop_timer] 
  [timers.bot_timer] *)
type timers = {
  mutable update : float;
  mutable drop_timer : float;
  mutable bot_timer : float;
}

(* mutable state of the active game 
   [game.over] is self-explanatory
   [game.player1] is the main player 
   [game.player2] is a optional second player 
   [game.gravity] corresponds to how fast the pieces move down
   [game.difficulty] is a chosen difficulty by the user, corresponds to how fast the bot is
   [game.timers] are the timers for the game *)
type game = {
  mutable over : bool;
  mutable player1 : Player.player;
  mutable player2 : Player.player option;
  mutable gravity : float;
  difficulty : float;
  mutable timers : timers;
}

(* [default_timer ()] returns [timer] where all fields are initialized as [0.] *)
val default_timer : unit -> timers

(* [run game] starts the game given [game]'s specs *)
val run : game -> unit 