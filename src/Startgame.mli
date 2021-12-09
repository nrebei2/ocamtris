(* mutable state of [game.timer]. [timers.update] is the interval in
  seconds from the last frame to the current one. [timers.drop_timer]
  is the interval in seconds from the last call of moving the pieces
  down. [timers.bot_timer] is the interval in seconds from the bot's
  last update *)
  type timers = {
    mutable update : float;
    mutable drop_timer : float;
    mutable bot_timer : float;
   }
    
   (* mutable state of the active game. [game.over] is [true] if the game
     is over, [false] otherwise. [game.players] is the gams's players.
     [game.gravity] corresponds to how fast the pieces move down.
     [game.difficulty] is a chosen difficulty by the user, corresponds to
     how fast the bot is. [game.timers] are the timers for the game *)
   type game = {
    mutable over : bool;
    mutable players : Player.player list;
    mutable gravity : float;
    mutable difficulty : float;
    mutable timers : timers;
   } 
    
   (* [default_timer ()] returns [timer] where all fields are initialized
     as [0.] *)
   val default_timer : unit -> timers
    
   (* current game the player is in*)
   val cur_game : game
    
   (* [run game] starts the game given [game]'s specifications *)
   val play_game : unit -> unit
   