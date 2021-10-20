open Game
open Gamemaster
open Player

type scene =
  | Menu
  | Play

type mode =
  | Alone
  | PvP
  | PvE

type difficulty =
  | Easy
  | Fair
  | Hard

(* initialize game given a mode and difficulty*)
let init_game m d =
  {
    over = false;
    players =
      (match m with
      | Alone -> generate_players 1 0
      | PvP -> generate_players 4 0
      | PvE -> generate_players 1 5);
    (* TODO change 0.1 to suitable value *)
    gravity = 0.1 /. 1.;
    difficulty =
      (match d with Easy -> 0.5 | Fair -> 0.2 | Hard -> 0.000000000075);
    timers = default_timer ();
  }

(* Execute the game *)
let () =
  Random.self_init ();
  let game = init_game Alone Hard in
  run game
