open Graphics
open Game
open Scene
open Gamemaster

(* Fixed framerate *)
let framerate = 30.

let fixed_update = ref 0.

let update = ref 0.

let rec game_loop () =
  fixed_update := !fixed_update +. Sys.time () -. !update;
  update := Sys.time ();
  (if !fixed_update > 1. /. framerate then
   match !cur_scene with
   | Game -> process_game cur_game
   | Settings -> process_settings_input ()
   | Leaderboard -> process_leaderboard_input ()
   | Menu -> process_menu_input ());
  game_loop ()

(* Execute the game *)
let () =
  Random.self_init ();
  open_graph " 600x800";
  open_scene ();
  game_loop ()
