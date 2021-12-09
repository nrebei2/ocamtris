open Graphics
open Player
open Leaderboard
open Drawing
open Startgame
open Scene
 
 
let leaderboard_name = ref "No Name"
 
let save_to_leaderboard players =
 match players with
 | [] -> ()
 | p1 :: _ ->
     let dir_separator = Filename.dir_sep in
     let leaderboard_file =
       "assets" ^ dir_separator ^ "leaderboard.json"
     in
     let scores =
       leaderboard_file |> from_json_file
       |> add_score (!leaderboard_name, p1.score)
     in
     let _ = save_leaderboard_file scores leaderboard_file in
     display_leaderboard 700
 
(* p is the loser *)
let rec game_over game p =
 game.over <- true;
 save_to_leaderboard game.players;
 moveto 350 700;
 draw_string "press r to retry, press q to quit"
 
and process_game_over game =
 let status = wait_next_event [ Key_pressed ] in
 if game.over then
   match status.key with
   | 'q' -> exit 0
   | 'r' -> Scene.switch_scene Menu
   | _ -> ()
 
and process_timers game =
 game.timers.drop_timer <-
   game.timers.drop_timer +. Sys.time () -. game.timers.update;
 
 game.timers.bot_timer <-
   game.timers.bot_timer +. Sys.time () -. game.timers.update;
 
 game.timers.update <- Sys.time ()
 
and process_game game player_name =
 if not game.over then (
   process_timers game;
 
   (if game.timers.drop_timer > game.gravity then
    try
      List.iter move_piece_down game.players;
      game.timers.drop_timer <- 0.
    with CantPlace p -> game_over game p);
   try
     process_human_players
       (List.filter (fun x -> x.bot = false) game.players);
     match List.filter (fun x -> x.bot = true) game.players with
     | [] -> ()
     | p -> 
         if game.timers.bot_timer > game.difficulty then (
           List.iter process_bot_player p;
           game.timers.bot_timer <- 0.)
   with CantPlace p -> game_over game p)
 else process_game_over game;
 leaderboard_name := player_name