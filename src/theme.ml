open Graphics

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

let clean = function
  | Background -> white
  | Outline -> black
  | Preview -> rgb 200 200 200
  | I -> cyan
  | O -> yellow
  | T -> magenta
  | S -> green
  | Z -> red 
  | J -> blue
  | L -> rgb 255 165 0

let retro = function
  | Background -> rgb 1 1 1
  | Outline -> rgb 1 1 1
  | Preview -> rgb 1 1 1
  | I -> rgb 1 1 1
  | O -> rgb 1 1 1
  | T -> rgb 1 1 1
  | S -> rgb 1 1 1
  | Z -> rgb 1 1 1 
  | J -> rgb 1 1 1
  | L -> rgb 1 1 1

let grayscale = function
  | Background -> white
  | Outline -> black
  | Preview -> rgb 220 220 220
  | I -> rgb 55 55 55
  | O -> rgb 70 70 70
  | T -> rgb 100 100 100
  | S -> rgb 120 120 120
  | Z -> rgb 150 150 150 
  | J -> rgb 180 180 180
  | L -> rgb 30 30 30
  
let colors_of_pallete = function
| Clean -> clean
| Retro -> retro
| Grayscale -> grayscale

let cur_theme = ref grayscale