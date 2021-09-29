open Graphics

open Game
open Board

let rec main () = 
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then exit 0
  else main ()

(* Execute the game *)
let () = open_graph " 600x800"; set_color black; draw_board (); main ()