open Graphics
open Button

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

(* default settings *)
let settings = { mode = Alone; diff = Easy; board_size = (22, 10) }

let difficulty_button diff bl tr =
  {
    bottom_left = bl;
    top_right = tr;
    draw =
      (fun () ->
        moveto (fst bl) (snd bl);
        draw_string
          (match diff with
          | Easy -> "Easy"
          | Fair -> "Fair"
          | Hard -> "Hard"));
    pressed_action = (fun () -> settings.diff <- diff);
  }

let mode_button mode bl tr =
  {
    bottom_left = bl;
    top_right = tr;
    draw =
      (fun () ->
        moveto (fst bl) (snd bl);
        draw_string
          (match mode with
          | Alone -> "Alone"
          | PvP -> "PvP"
          | PvE -> "PvE"));
    pressed_action = (fun () -> settings.mode <- mode);
  }

