open Graphics
open Player
open Leaderboard
open Drawing

type timers = {
  mutable update : float;
  mutable drop_timer : float;
  mutable bot_timer : float;
}

type game = {
  mutable over : bool;
  mutable players : player list;
  mutable gravity : float;
  mutable difficulty : float;
  mutable timers : timers;
}

let default_timer () = { update = 0.; drop_timer = 0.; bot_timer = 0. }

let cur_game =
  {
    over = true;
    players = [];
    gravity = 0.;
    difficulty = 0.;
    timers = default_timer ();
  }

(* initialize game given current mode and difficulty*)
let init_game () =
  Tetromino.reset_bag ();
  Random.self_init ();
  cur_game.over <- false;
  cur_game.players <-
    (match Settings.settings.mode with
    | Alone -> generate_players 1 0
    | PvP -> generate_players 2 0
    | PvE -> generate_players 1 1);
  cur_game.gravity <- 0.1 /. 1.;
  cur_game.difficulty <-
    (match Settings.settings.diff with
    | Easy -> 0.5
    | Fair -> 0.2
    | Hard -> 0.000000000075);
  cur_game.timers <- default_timer ()

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

(* p is the loser *)
let rec game_over game p =
  game.over <- true;
  clear_graph ();
  save_to_leaderboard game.players;
  moveto 350 700;
  draw_string "press r to retry, press q to quit"

and process_game_over game =
  let status = wait_next_event [ Key_pressed ] in
  if game.over then
    match status.key with
    | 'q' -> exit 0
    | 'r' -> play_game ()
    | _ -> ()

and process_timers game =
  game.timers.drop_timer <-
    game.timers.drop_timer +. Sys.time () -. game.timers.update;

  game.timers.bot_timer <-
    game.timers.bot_timer +. Sys.time () -. game.timers.update;

  game.timers.update <- Sys.time ()

and process_game game =
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
  else process_game_over game

and init_screen game =
  resize_window (650 * List.length game.players) 800;
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
  init_game ();
  init_screen cur_game