open Settings
open Gamemaster
open Leaderboard

type scene =
  | Menu
  | Settings
  | Leaderboard
  | Game

let cur_scene = ref Game

(*Draws the button options on menu screen*)
let draw_buttons () = ()

(*Called from open_menu and processes input from user of mouse clicks*)
let process_requests () = ()

(*Called from open_menu and draws the different buttons*)
let draw_menu () = ()

(*Opens the initial menu screen*)
let open_menu () = ()
(* open_graph (Printf.sprintf " %dx800" (650 * List.length
   game.players)) *)


let open_scene () =
  match !cur_scene with
  | Menu -> open_menu ()
  | Settings -> open_settings ()
  | Game -> play_game ()
  | Leaderboard -> display_leaderboard 700
  (* | Settings -> open_settings () *)
(* Random.self_init (); let game = init_game PvE Hard in run game *)
