(** Representation of the menu screen.

    This module represents the menu screen in which the user can choose
    to start the game, access settings, view the leaderboard, or quit
    the game.*)

(** [scene] is the state of the user that represents whether the user is
    in the menu screen *)
type scene =
  | Menu
  | Settings
  | Leaderboard
  | Game

val cur_scene : scene ref
(** [cur_scene] is the scene that is currently being displayed to the
    player. *)

val open_scene : unit -> unit
(** [open_scene] opens the current scene. [open_scene] opens the menu if
    the scene is of type Menu, and runs the game if scene is of type
    Game. Additionally, if the scene is of type leaderboard, it displays
    the current leaderboard.*)

val switch_scene : scene -> unit
(** [switch_scene s] takes a scene [s] and sets the current scene to
    scene [s]. It then opens the current scene.*)

val process_settings_input : unit -> unit
(** [process_settings_input] processes user mouse clicks for all the
    buttons in the Settings scene.*)

val process_menu_input : unit -> unit
(** [process_menu_input] processes user mouse clicks for all the buttons
    in the Menu scene.*)

val process_leaderboard_input : unit -> unit
(** [process_leaderboard_input] processes user mouse clicks for all the
    buttons in the Leaderboard scene.*)
