open Graphics
open Button
open Yojson.Basic.Util

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

let settings_file = "assets/settings.json"

let config_of_json json : settings =
  let difficulty =
    match json |> member "difficulty" |> to_string with
    | "Easy" -> Easy
    | "Fair" -> Fair
    | "Hard" -> Hard
    | _ -> failwith "invalid difficulty"
  in
  let mode =
    match json |> member "mode" |> to_string with
    | "Alone" -> Alone
    | "PvP" -> PvP
    | "PvE" -> PvE
    | _ -> failwith "invalid against"
  in
  let theme =
    match json |> member "theme" |> to_string with
    | "Clean" -> Clean
    | "Retro" -> Retro
    | "Grayscale" -> Grayscale
    | "Soft" -> Soft
    | _ -> failwith "invalid theme"
  in
  { diff = difficulty; mode; board_size = (22, 10); theme }

let from_json_file filename : settings =
  let json_contents = Yojson.Basic.from_file filename in
  try config_of_json json_contents
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let to_json (settings : settings) : Yojson.t =
  (* let theme_str = match config.theme with | Monochrome ->
     "monochrome" | Classic -> "classic" in *)
  let difficulty_str =
    match settings.diff with
    | Easy -> "Easy"
    | Fair -> "Fair"
    | Hard -> "Hard"
  in
  let mode_str =
    match settings.mode with
    | Alone -> "Alone"
    | PvP -> "PvP"
    | PvE -> "PvE"
  in
  let theme_str =
    match settings.theme with
    | Clean -> "Clean"
    | Retro -> "Retro"
    | Grayscale -> "Grayscale"
    | Soft -> "Soft"
  in
  `Assoc
    [
      ("mode", `String mode_str);
      ("difficulty", `String difficulty_str);
      ("theme", `String theme_str);
      
    ]

let save_settings_file (settings : settings) (filename : string) =
  Yojson.to_file filename (to_json settings)

let settings = from_json_file settings_file
(* load existing settings *)

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
        save_settings_file settings settings_file;
        draw_border bl tr);
  }

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
        save_settings_file settings settings_file;
        draw_border bl tr);
  }