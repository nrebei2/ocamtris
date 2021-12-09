open Graphics
open Player
open Leaderboard
open Drawing
 
 
 
type timers = {
 mutable update : float;
 mutable drop_timer : float;
 mutable bot_timer : float;
}
let default_timer () = { update = 0.; drop_timer = 0.; bot_timer = 0. }
 
type game = {
 mutable over : bool;
 mutable players : player list;
 mutable gravity : float;
 mutable difficulty : float;
 mutable timers : timers;
}
 
let cur_game =
 {
   over = true;
   players = [];
   gravity = 0.;
   difficulty = 0.;
   timers = default_timer ();
 }
 
let init_game () =
 Tetromino.reset_bag ();
 Random.self_init ();
 cur_game.over <- false;
 cur_game.players <-
   (match Settings.settings.mode with
   | Alone -> generate_players 1 0
   | PvP -> generate_players 2 0
   | PvE -> generate_players 1 1);
 cur_game.gravity <-
   (match Settings.settings.diff with
   | Easy -> 0.1
   | Fair -> 0.075
   | Hard -> 0.05)
   /. 1.;
 cur_game.difficulty <-
   (match Settings.settings.diff with 
   | Easy -> 0.5
   | Fair -> 0.2
   | Hard -> 0.000000000075);
 cur_game.timers <- default_timer ()
 
let init_screen game =
 resize_window (650 * List.length game.players) 800;
 draw_title ();
 List.iter
   (fun x ->
     draw_score x;
     draw_outline x.board_pos;
     draw_next_piece x.board_pos;
     draw_instructions x;
     spawn_piece x)
   game.players
let play_game () =
 init_game ();
 init_screen cur_game
