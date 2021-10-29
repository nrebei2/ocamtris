open Graphics
open Board
open Tetromino
open Player
open Leaderboard

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

(* TODO: Respresent leaderboard as json and use Yojson to read/write
   data *)
let rec display_leaderboard players =
  match players with
  | [] -> ()
  | p1 :: _ ->
      moveto 100 700;
      draw_string "leaderboard:";
      let dir_separator = Filename.dir_sep in
      let leaderboard_file =
        "assets" ^ dir_separator ^ "leaderboard.json"
      in
      let scores =
        leaderboard_file |> from_json_file
        |> add_score ("temp name", p1.score)
      in
      let _ = save_leaderboard_file scores leaderboard_file in
      let _, text_height = text_size "leaderboard:" in
      let rec draw_scores vertical_pos scores_lst =
        match scores_lst with
        | [] -> ()
        | h :: t ->
            moveto 100 vertical_pos;
            let player_name, player_score = h in
            draw_string (player_name ^ ": " ^ string_of_int player_score);
            draw_scores (vertical_pos - text_height) t
      in

      scores |> draw_scores (700 - text_height)

and reset game =
  reset_bag ();
  game.players <- List.map reset_player game.players

(* p is the loser *)
and game_over game p =
  game.over <- true;
  clear_graph ();
  reset game;

  display_leaderboard game.players;
  moveto 350 700;
  draw_string "press r to retry, press q to quit";
  process_game_over_requests game

and process_game_over_requests game =
  (let event2 = wait_next_event [ Key_pressed ] in
   if game.over then
     match event2.key with 'q' -> exit 0 | 'r' -> run game | _ -> ());
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

and run game =
  game.over <- false;
  init_screen game;
  process_players game
    (List.filter (fun x -> x.bot = false) game.players)
    (List.filter (fun x -> x.bot = true) game.players)
