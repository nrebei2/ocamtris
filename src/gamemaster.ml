open Graphics
open Tetromino
open Player
open Leaderboard
open Drawing
open Settings

type timers = {
  mutable update : float;
  mutable drop_timer : float;
  mutable bot_timer : float;
}

(* TODO: add bag to game type, as it is esentially a global atm *)

type game = {
  mutable over : bool;
  mutable players : player list;
  mutable gravity : float;
  difficulty : float;
  mutable timers : timers;
}

let default_timer () = { update = 0.; drop_timer = 0.; bot_timer = 0. }

(* initialize game given current mode and difficulty*)
let init_game () =
  {
    over = false;
    players =
      (match Settings.settings.mode with
      | Alone -> generate_players 1 0
      | PvP -> generate_players 1 1
      | PvE -> generate_players 1 1);
    (* TODO change 0.1 to suitable value *)
    gravity = 0.1 /. 1.;
    difficulty =
      (match Settings.settings.diff with
      | Easy -> 0.5
      | Fair -> 0.2
      | Hard -> 0.000000000075);
    timers = default_timer ();
  }

let cur_game = ref None

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
        |> add_score ("temp name", p1.score)
      in
      let _ = save_leaderboard_file scores leaderboard_file in

      display_leaderboard 700

let rec reset game =
  reset_bag ();
  game.players <- List.map reset_player game.players

(* p is the loser *)
and game_over game p =
  game.over <- true;
  clear_graph ();
  reset game;

  save_to_leaderboard game.players;
  moveto 350 700;
  draw_string "press r to retry, press q to quit";
  process_game_over_requests game

and process_game_over_requests game =
  (let event2 = wait_next_event [ Key_pressed ] in
   if game.over then
     match event2.key with
     | 'q' -> exit 0
     | 'r' -> play_game ()
     | _ -> ());
  process_game_over_requests game

and process_players game p_list bot_list =
  if not game.over then (
    (* Pieces drop *)
    game.timers.drop_timer <-
      game.timers.drop_timer +. Sys.time () -. game.timers.update;

    game.timers.bot_timer <-
      game.timers.bot_timer +. Sys.time () -. game.timers.update;

    game.timers.update <- Sys.time ();

    (if game.timers.drop_timer > game.gravity then
     try
       List.iter move_piece_down (bot_list @ p_list);
       game.timers.drop_timer <- 0.
     with CantPlace p -> game_over game p);
    begin
      try
        process_human_players p_list;
        match bot_list with
        | [] -> ()
        | p ->
            if game.timers.bot_timer > game.difficulty then (
              List.iter process_bot_player p;
              game.timers.bot_timer <- 0.)
      with CantPlace p -> game_over game p
    end;

    process_players game p_list bot_list)

and init_screen game =
  open_graph (Printf.sprintf " %dx800" (650 * List.length game.players));
  clear_graph ();
  draw_title ();
  draw_next_piece ();
  draw_instructions ();
  List.iter
    (fun x ->
      draw_score x;
      draw_outline x.board_pos;
      spawn_piece x)
    game.players

and play_game () =
  let game = init_game () in
  cur_game := Some game;
  game.over <- false;
  init_screen game;
  process_players game
    (List.filter (fun x -> x.bot = false) game.players)
    (List.filter (fun x -> x.bot = true) game.players)
