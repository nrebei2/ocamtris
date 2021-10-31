open Graphics 
open Game
open Settings
open Scene

let process_inputs status = 
  match !cur_scene with
  | Menu -> ()

(* Execute the game *)
let () =
  Random.self_init ();
  open_scene ();
  loop_at_exit [ Button_down; Key_pressed ] process_inputs;
  
