open Graphics
let rows = 20
let columns = 10
let board_pos = 100, 50
let tile_size = 30
let draw_board () = 
  for i = 0 to columns do
    moveto (fst board_pos + tile_size*i) (snd board_pos);
    lineto (fst board_pos + tile_size*i) (snd board_pos + tile_size*rows);
  done;
  for j = 0 to rows do 
    moveto (fst board_pos) (snd board_pos + tile_size*j);
    lineto (fst board_pos + tile_size*columns) (snd board_pos + tile_size*j);
  done 