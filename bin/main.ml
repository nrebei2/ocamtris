open Graphics
open Game
open Settings
open Scene
open Gamemaster

let rec game_loop () =
  begin
    match !cur_scene with
    | Game -> process_game cur_game
    | Settings -> process_settings_input ()
    | Leaderboard -> process_leaderboard_input ()
    | Menu -> process_menu_input ()
  end;
  game_loop ()

(* Execute the game *)
let () =
  Random.self_init ();
  open_graph " 600x800";
  open_scene ();
  game_loop ()
