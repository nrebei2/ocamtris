type mode =
  | Alone
  | PvP
  | PvE

type difficulty =
  | Easy
  | Fair
  | Hard

type settings = {
  mutable mode : mode;
  mutable diff : difficulty;
  mutable board_size : int * int;
}

val settings : settings

val open_settings : unit -> unit