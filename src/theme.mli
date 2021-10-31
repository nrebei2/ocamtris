type palletes =
  | Clean
  | Retro
  | Grayscale

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

val colors_of_pallete : palletes -> (colors -> Graphics.color)

val cur_theme : (colors -> Graphics.color) ref