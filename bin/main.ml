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

let init_game m d =
  {
    over = false;
    player1 = default_player ();
    player2 =
      (match m with
      | Alone -> None
      | PvP -> Some (default_player_2 ())
      | PvE -> Some (default_bot ()));
    (* TODO change 0.1 to suitable value *)
    gravity = 0.1 /. 1.;
    difficulty =
      (match d with Easy -> 0.5 | Fair -> 0.2 | Hard -> 0.075);
    timers = default_timer ();
  }

(* Execute the game *)
let () =
  Random.self_init ();
  let game = init_game PvE Hard in
  run game
