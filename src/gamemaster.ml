open Graphics
open Board
open Tetromino
open Player

(* TODO: think about adding bag to game type *)

type timers = {
  mutable update : float;
  mutable drop_timer : float;
  mutable bot_timer : float;
}

type game = {
  mutable over : bool;
  mutable player1 : player;
  mutable player2 : player option;
  mutable gravity : float;
  difficulty : float;
  mutable timers : timers;
}

let default_timer () = { update = 0.; drop_timer = 0.; bot_timer = 0. }

(* TODO: Respresent leaderboard as json and use Yojson to read/write
   data *)
let rec display_leaderboard () =
  moveto 100 100;
  draw_string "leaderboard here"

and reset game =
  reset_bag ();
  game.player1 <- default_player ();
  match game.player2 with
  | None -> ()
  | Some p ->
      if p.bot then game.player2 <- Some (default_bot ())
      else game.player2 <- Some (default_player_2 ())

(* p is the loser *)
and game_over game p =
  game.over <- true;
  clear_graph ();
  reset game;

  display_leaderboard ();
  moveto 350 100;
  draw_string "press r to retry, press q to quit";
  process_game_over_requests game

and process_game_over_requests game =
  (let event2 = wait_next_event [ Key_pressed ] in
   if game.over then
     match event2.key with 'q' -> exit 0 | 'r' -> run game | _ -> ());
  process_game_over_requests game

and process_players game p_list bot =
  if not game.over then (
    game.timers.drop_timer <-
      game.timers.drop_timer +. Sys.time () -. game.timers.update;

    game.timers.bot_timer <-
      game.timers.bot_timer +. Sys.time () -. game.timers.update;

    game.timers.update <- Sys.time ();

    (if game.timers.drop_timer > game.gravity then
     try
       List.iter move_piece_down
         ((match bot with None -> [] | Some p -> [ p ]) @ p_list);
       game.timers.drop_timer <- 0.
     with CantPlace p -> game_over game p);

    begin
      try
        process_human_players p_list;
        match bot with
        | None -> ()
        | Some p ->
            if game.timers.bot_timer > game.difficulty then (
              process_bot_player p;
              game.timers.bot_timer <- 0.)
      with CantPlace p -> game_over game p
    end;

    process_players game p_list bot)

and init_screen game =
  begin
    match game.player2 with
    | None -> open_graph " 650x800"
    | Some _ -> open_graph " 1350x800"
  end;
  clear_graph ();
  (* draw_title (); *)
  draw_outline game.player1.board_pos;
  spawn_piece game.player1;
  match game.player2 with
  | None -> ()
  | Some p ->
      draw_outline p.board_pos;
      spawn_piece p

and run game =
  game.over <- false;
  init_screen game;
  match game.player2 with
  | None -> process_players game [ game.player1 ] None
  | Some p2 ->
      if p2.bot then process_players game [ game.player1 ] (Some p2)
      else process_players game [ game.player1; p2 ] None
