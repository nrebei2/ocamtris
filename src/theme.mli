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

val theme_button :
  Settings.palletes -> int * int -> int * int -> Button.button
