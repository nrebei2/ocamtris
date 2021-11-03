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

val difficulty_button : difficulty -> int * int -> Button.button

val mode_button : mode -> int * int -> Button.button
