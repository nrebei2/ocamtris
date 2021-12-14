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
  (if Settings.settings.mode = Settings.Alone then
   save_to_leaderboard game.players
  else
    let score = ref min_int in
    let place = ref 0 in
    List.iteri
      (fun i p' ->
        if p = p' then ()
        else if p.score > !score then (
          score := p.score;
          place := i + 1)
        else ())
      game.players;
    clear_graph ();
    set_color black;
    moveto 100 700;
    draw_string
      ("Player " ^ string_of_int !place ^ " wins with score "
     ^ string_of_int !score ^ "!"));

  moveto 350 700;
  draw_string "press r to retry";
  moveto 350 685;
  draw_string "press m to go back to menu";
  moveto 350 670;
  draw_string "press q to quit"

and process_game_over game =
  let status = wait_next_event [ Key_pressed ] in
  if game.over then
    match status.key with
    | 'q' -> exit 0
    | 'r' -> play_game ()
    | 'm' -> Scene.switch_scene Menu
    | _ -> ()

and process_timers game =
  game.timers.drop_timer <-
    game.timers.drop_timer +. Sys.time () -. game.timers.update;

  game.timers.bot_timer <-
    game.timers.bot_timer +. Sys.time () -. game.timers.update;

  game.timers.update <- Sys.time ();

  game.gravity <- game.gravity /. 1.000001

and display_garbage clear p =
  set_color white;
  fill_rect (fst p.board_pos + 100) (snd p.board_pos - 15) 150 13;
  if clear then ()
  else (
    moveto (fst p.board_pos + 100) (snd p.board_pos - 15);
    set_color black;
    draw_string ("Garbage incoming: " ^ string_of_int p.garbage_info.inc))

and manage_garbage players =
  let f i p =
    if p.garbage_info.drop = true then (
      Board.add_garbage p.garbage_info.inc p.board;
      Board.draw_board p.board p.board_pos;
      p.garbage_info.drop <- false;
      p.garbage_info.inc <- 0;
      display_garbage true p);
    if p.garbage_info.send > 0 then (
      let nxt_p = List.nth players ((i + 1) mod List.length players) in
      nxt_p.garbage_info.inc <-
        nxt_p.garbage_info.inc + p.garbage_info.send;
      display_garbage false nxt_p;
      p.garbage_info.send <- 0)
  in
  List.iteri f players

and process_game game player_name =
  if not game.over then (
    begin
      match Settings.settings.mode with
      | Alone -> ()
      | _ -> manage_garbage game.players
    end;
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
