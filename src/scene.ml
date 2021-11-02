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

let string_of_scene scene =
  match scene with
  | Menu -> "Menu"
  | Settings -> "Settings"
  | Leaderboard -> "Leaderboard"
  | Game -> "Ocamtris"

let rec settings_buttons () =
  [
    theme_button Theme.Grayscale (100, 600) (200, 700);
    theme_button Theme.Clean (300, 600) (400, 700);
    difficulty_button Easy (100, 100) (200, 200);
    mode_button PvP (300, 100) (400, 200);
    scene_button Game (500, 100) (600, 200);
  ]

and menu_buttons () = []

and process_settings_input () =
  process_button_input (settings_buttons ())

and open_scene () =
  set_window_title (string_of_scene !cur_scene);
  match !cur_scene with
  | Menu ->
      resize_window 650 800;
      List.iter (fun b -> b.draw ()) (menu_buttons ())
  | Settings ->
      resize_window 650 800;
      List.iter (fun b -> b.draw ()) (settings_buttons ())
  | Game -> play_game ()
  | Leaderboard ->
      resize_window 600 800;
      display_leaderboard 700

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
        draw_string (string_of_scene scene));
    pressed_action = (fun () -> switch_scene scene);
  }
