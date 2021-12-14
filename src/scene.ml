open Settings
open Startgame
open Leaderboard
open Graphics
open Button
open Theme

type scene =
  | Menu
  | Settings
  | Leaderboard
  | Game

let cur_scene = ref Menu

let string_of_scene scene =
  match scene with
  | Menu -> "Menu"
  | Settings -> "Settings"
  | Leaderboard -> "Leaderboard"
  | Game -> "Play"

let theme_button_size = (100, 100)

let padding = 50

let generate_theme_buttons (themes : palletes list) : button list =
  let cur_x, cur_y = (125, 600) in
  let width, height = theme_button_size in
  List.mapi
    (fun index theme ->
      theme_button theme
        (cur_x + (index * (width + padding)), cur_y)
        (cur_x + (index * (width + padding)) + width, cur_y + height))
    themes

let rec settings_buttons () =
  generate_theme_buttons [ Grayscale; Clean; Soft ]
  @ [
      difficulty_button Easy (100, 150);
      difficulty_button Fair (100, 125);
      difficulty_button Hard (100, 100);
      mode_button Alone (300, 150);
      mode_button PvP (300, 125);
      mode_button PvE (300, 100);
      scene_button Game (500, 125);
      scene_button Menu (500, 100);
    ]

and menu_buttons () =
  [
    scene_button Game (315, 700);
    scene_button Settings (305, 500);
    scene_button Leaderboard (300, 300);
    quit_button ()
  ]

and quit_button () =
  {
    name = "quit";
    bottom_left = (315, 100);
    top_right = (350, 115);
    draw =
      (fun () ->
        set_color black;
        moveto 315 100;
        draw_string "Quit");
    pressed_action = (fun () -> exit 0);
  }

and leaderboard_buttons () = [ scene_button Menu (500, 100) ]

and process_settings_input () =
  process_button_input (settings_buttons ())

and process_menu_input () = process_button_input (menu_buttons ())

and process_leaderboard_input () =
  process_button_input (leaderboard_buttons ())

and open_scene () =
  set_window_title (string_of_scene !cur_scene);
  match !cur_scene with
  | Menu ->
      resize_window 650 800;
      List.iter (fun b -> b.draw ()) (menu_buttons ())
  | Settings ->
      resize_window 650 800;
      List.iter (fun b -> b.draw ()) (settings_buttons ());
      moveto 310 725;
      draw_string "Themes"
  | Game -> Startgame.play_game ()
  | Leaderboard ->
      resize_window 650 800;
      display_leaderboard 700;
      List.iter (fun b -> b.draw ()) (leaderboard_buttons ())

and switch_scene scene =
  cur_scene := scene;
  open_scene ()

and scene_button scene bl =
  let text = string_of_scene scene in
  let tr =
    (fst (text_size text) + fst bl, snd (text_size text) + snd bl)
  in
  {
    name = "scene";
    bottom_left = bl;
    top_right = tr;
    draw =
      (fun () ->
        set_color black;
        moveto (fst bl) (snd bl);
        draw_string text);
    pressed_action = (fun () -> switch_scene scene);
  }

let change_bool =
  let boolean = ref false in
  fun () ->
    boolean := not !boolean;
    !boolean
