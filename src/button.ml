open Graphics

type button = {
  bottom_left : int * int;
  top_right : int * int;
  draw : unit -> unit;
  pressed_action : unit -> unit;
}

let theme_button palette bl tr =
  {
    bottom_left = bl;
    top_right = tr;
    draw =
      (fun () ->
        let colors = Theme.[ I; O; T; S; Z; J ] in
        List.iteri
          (fun i c ->
            set_color ((Theme.colors_of_pallete palette) c);
            let height = (snd tr - snd bl) / List.length colors in
            fill_rect (fst bl)
              (snd bl + (height * i))
              (fst tr - fst bl)
              height)
          colors);
    pressed_action = (fun () -> Theme.cur_theme := (Theme.colors_of_pallete palette));
  }

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