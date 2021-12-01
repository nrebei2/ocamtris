type mode =
  | Alone
  | PvP
  | PvE

type difficulty =
  | Easy
  | Fair
  | Hard

type palletes =
  | Clean
  | Retro
  | Grayscale
  | Soft

type settings = {
  mutable mode : mode;
  mutable diff : difficulty;
  mutable board_size : int * int;
  mutable theme : palletes;
}

val settings_file : string
(** [settings_file] is the filename containing the current settings. *)

val settings : settings

val difficulty_button : difficulty -> int * int -> Button.button

val mode_button : mode -> int * int -> Button.button

val from_json_file : string -> settings
(** [from_json_file f] are the settings that json file [f] represents.
    Requires: [f] is the filename of a valid JSON settings
    representation. *)

val save_settings_file : settings -> string -> unit
(** [save_config_file s f] exports settings [s] to filename [f]. *)
