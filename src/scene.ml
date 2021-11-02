open Settings
open Gamemaster
open Leaderboard
open Graphics
open Button
open Theme

type scene =
  | Menu
  | Settings
  | Leaderboard
  | Game

let cur_scene = ref Settings

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


let rec settings_buttons () =
  [
    theme_button Theme.Grayscale (100, 600) (200, 700);
    theme_button Theme.Clean (300, 600) (400, 700);
    difficulty_button Easy (100, 100) (200, 200);
    mode_button PvP (300, 100) (400, 200);
    scene_button Game (500, 100) (600, 200);
  ]

and process_settings_input () = process_button_input (settings_buttons ())

and open_settings () =
  resize_window 650 800;
  set_window_title "Settings";
  List.iter (fun b -> b.draw ()) (settings_buttons ())

and open_scene () =
  clear_graph ();
  match !cur_scene with
  | Menu -> open_menu ()
  | Settings ->
      open_settings ()
        
  | Game -> play_game ()
  | Leaderboard ->
      resize_window 600 800;
      display_leaderboard 700

(* | Settings -> open_settings () *)
(* Random.self_init (); let game = init_game PvE Hard in run game *)

and switch_scene scene =
  cur_scene := scene;
  open_scene ()

and scene_button scene bl tr =
  {
    bottom_left = bl;
    top_right = tr;
    draw =
      (fun () ->
        moveto (fst bl) (snd bl);
        draw_string
          (match scene with
          | Game -> "Game"
          | Leaderboard -> "Leaderboard"
          | Menu -> "Menu"
          | Settings -> "Settings"));
    pressed_action = (fun () -> switch_scene scene);
  }

