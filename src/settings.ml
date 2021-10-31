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

type settings = 
{
  mutable mode : mode;
  mutable diff : difficulty;
  mutable board_size : int * int
}

let settings =
  {
    mode = Alone;
    diff = Easy;
    board_size = 22, 10
  }

let buttons = [ theme_button Theme.Grayscale (100, 100) (200, 200) ]

let draw_settings () =
  set_window_title "Settings";
  List.iter (fun b -> b.draw ()) buttons

let rec process_settings_input status =
  if status.button then
    match get_button (mouse_pos ()) buttons with
    | None -> ()
    | Some b ->
        b.pressed_action ()

let open_settings () =
  loop_at_exit [ Button_down ] process_settings_input;
  open_graph " 650x800";
  clear_graph ();
  draw_settings ()
