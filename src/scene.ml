open Graphics
open Gamemaster
open Player

type scene =
  | Menu
  | Play

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

(*[open_scene] opens the main menu if the current scene is Menu. If the
  current scene is Play, it starts the game at default settings.*)
let open_scene scene =
  match scene with Menu -> open_menu () | Play -> ()
(* Random.self_init (); let game = init_game PvE Hard in run game *)
