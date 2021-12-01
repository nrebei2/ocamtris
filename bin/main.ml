open Graphics
open Game
open Scene
open Gamemaster

(* Fixed framerate *)
let framerate = 30.

let fixed_update = ref 0.

let update = ref 0.

let player_name = ref "No Name"

let rec game_loop () =
  fixed_update := !fixed_update +. Sys.time () -. !update;
  update := Sys.time ();
  (if !fixed_update > 1. /. framerate then
   match !cur_scene with
   | Game -> process_game cur_game !player_name
   | Settings -> process_settings_input ()
   | Leaderboard -> process_leaderboard_input ()
   | Menu -> process_menu_input ());
  game_loop ()

(* Execute the game *)
let () =
  Random.self_init ();
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to Ocamtris.\n";
  print_endline "Please enter your name.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name ->
      print_endline ("Hello " ^ name ^ "! Opening game window...");
      player_name := name;
      open_graph " 600x800";
      open_scene ();
      game_loop ()
