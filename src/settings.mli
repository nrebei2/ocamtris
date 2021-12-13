type mode =
  | Alone
  | PvP
  | PvE

(** [mode] is who the player is playing against. [Alone] is 1 player,
    [PvP] is two players on the keyboard, and [PvE] is a player against
    a bot. *)

type difficulty =
  | Easy
  | Fair
  | Hard

(** [difficulty] is how hard the game is, meaning how fast the pieces
    drop. *)

type palletes =
  | Clean
  | Retro
  | Grayscale
  | Soft

(** [palletes] represents the different possible color palletes that can
    be played. *)

type settings = {
  mutable mode : mode;
  mutable diff : difficulty;
  mutable board_size : int * int;
  mutable theme : palletes;
}
(** [settings] contains information about who the player is playing
    against, how hard the game is, how large the board is, and the
    current game theme. *)

val settings_file : string
(** [settings_file] is the filename containing the current settings. *)

val settings : settings
(** [settings] is the current settings information for the game, which
    is stored in [assets/settings.json]. *)

val difficulty_button : difficulty -> int * int -> Button.button
(** [difficulty_botton d (i1, i2) creates a difficulty button with text based on \[d\] and size \[(i1, i2)\]] *)

val mode_button : mode -> int * int -> Button.button
(** [difficulty_botton d (i1, i2) creates a difficulty button with text based on \[d\] and size \[(i1, i2)\]] *)

val from_json_file : string -> settings
(** [from_json_file f] are the settings that json file [f] represents.
    Requires: [f] is the filename of a valid JSON settings
    representation. *)

val save_settings_file : settings -> string -> unit
(** [save_config_file s f] exports settings [s] to filename [f]. *)
