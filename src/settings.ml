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

let difficulty_button diff bl =
  let text =
    match diff with Easy -> "Easy" | Fair -> "Fair" | Hard -> "Hard"
  in
  let tr =
    (fst (text_size text) + fst bl, snd (text_size text) + snd bl)
  in
  {
    name = "diff";
    bottom_left = bl;
    top_right = tr;
    draw =
      (fun () ->
        clear_border bl tr;
        if diff = settings.diff then draw_border bl tr;
        set_color black;
        moveto (fst bl) (snd bl);
        draw_string text);
    pressed_action =
      (fun () ->
        settings.diff <- diff;
        draw_border bl tr);
  }

let mode_button mode bl =
  let text =
    match mode with Alone -> "Alone" | PvP -> "PvP" | PvE -> "PvE"
  in
  let tr =
    (fst (text_size text) + fst bl, snd (text_size text) + snd bl)
  in
  {
    name = "mode";
    bottom_left = bl;
    top_right = tr;
    draw =
      (fun () ->
        clear_border bl tr;
        if mode = settings.mode then draw_border bl tr;
        set_color black;
        moveto (fst bl) (snd bl);
        draw_string text);
    pressed_action =
      (fun () ->
        settings.mode <- mode;
        draw_border bl tr);
  }
