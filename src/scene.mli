(** Representation of the menu screen.

    This module represents the menu screen in which the user can choose
    to start the game, access settings, view the leaderboard, or quit
    the game.*)

(* [scene] is the state of the user that represents whether the user is
   in the menu screen *)
type scene =
  | Menu
  | Settings
  | Leaderboard
  | Game

val cur_scene : scene ref

(* [open_scene] opens the menu if the scene is of type Menu, and runs
   the game if scene is of type Game.*)
val open_scene : unit -> unit

val switch_scene : scene -> unit

val process_settings_input : unit -> unit
