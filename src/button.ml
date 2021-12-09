open Graphics

type button = {
  name : string;
  bottom_left : int * int;
  top_right : int * int;
  draw : unit -> unit;
  pressed_action : unit -> unit;
}
(** Reresentation type for a button. [bottom_left] is the buttons
    position in the graphics screen, similarly for [top_right]. [draw]
    is a function that draws the button to the screen. [press_action] is
    a function that should be called when the button is pressed. *)

(** [get_button mouse_pos button_list] returns the first button [b] in
    [button_list] that [mouse_pos] in inside the bounds of. *)
let rec get_button m_pos = function
  | [] -> None
  | b :: t ->
      if
        fst m_pos >= fst b.bottom_left
        && fst m_pos <= fst b.top_right
        && snd m_pos >= snd b.bottom_left
        && snd m_pos <= snd b.top_right
      then Some b
      else get_button m_pos t

(** [process_button_input buttons] processes user mouse cliks using
    [Graphics.status] for [buttons]*)
let process_button_input buttons =
  let status = wait_next_event [ Button_down ] in
  if status.button then
    match get_button (mouse_pos ()) buttons with
    | None -> ()
    | Some b ->
        b.pressed_action ();
        List.iter
          (fun b2 ->
            if b2.name = b.name && b.name <> "scene" then b2.draw ())
          buttons

(** [draw_border bl tr] draws a black button border using coordinates
    [bl] for bottom left and [tr] for top right. *)
let draw_border bl tr =
  set_line_width 3;
  set_color black;
  draw_rect (fst bl) (snd bl) (fst tr - fst bl) (snd tr - snd bl);
  set_line_width 1

(** [clear_border bl tr] draws a white button border using coordinates
    [bl] for bottom left and [tr] for top right. *)
let clear_border bl tr =
  set_line_width 3;
  set_color white;
  draw_rect (fst bl) (snd bl) (fst tr - fst bl) (snd tr - snd bl);
  set_line_width 1
