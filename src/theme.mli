(** each color is a different part of the came that needs a distinct
    color, i.e. tetrominoes and the background. *)
type colors =
  | Background
  | Outline
  | Preview
  | I
  | O
  | T
  | S
  | Z
  | J
  | L

val colors_of_pallete : Settings.palletes -> colors -> Graphics.color
(** [colors_of_pallete ps c] outputs the color for piece c in pallete
    ps. *)

val theme_button :
  Settings.palletes -> int * int -> int * int -> Button.button
(** [theme_button ps (i1, i2) (i1, i2) outputs a button that previews the theme \[ps\].] *)
