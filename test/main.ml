(* What we did: stuff *)

open OUnit2
open Game
open Tetromino
open Board
open Bot
open Leaderboard

(** Adopted from A2: [pp_string s] pretty-prints string [s]. *)
let pp_string s = s

(** Adopted from A2: [pp_list pp_elt lst] pretty-prints list [lst],
    using [pp_elt] to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* [pp_score (n, i)] pretty-prints a score with name [n] and score
   [i] *)
let pp_score (n, i) = n ^ ": " ^ string_of_int i

(** [pp_board pp_elt board] pretty-prints board [board] using [pp_list]
    [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_board pp_elt board =
  let pp_list pp_elt sep lst =
    let pp_elts lst sep =
      let rec loop n acc sep = function
        | [] -> acc ^ "n"
        | [ h ] -> acc ^ pp_elt h
        | h1 :: (h2 :: t as t') ->
            loop (n + 1) (acc ^ pp_elt h1 ^ sep) sep t'
      in
      loop 0 "" sep lst
    in
    "[" ^ pp_elts lst sep ^ "]"
  in
  "\n"
  ^ pp_list pp_elt "\n"
      (board
      |> Array.map (Array.map (String.make 1))
      |> Array.to_list |> List.map Array.to_list
      |> List.map (pp_list pp_elt "; "))

let board_test name board f expected printer =
  let board = Array.copy board in
  name >:: fun _ -> assert_equal expected (f board) ~printer

let em_2x2 = Array.make_matrix 2 2 ' '

let em_3x3 = Array.make_matrix 3 3 ' '

let test_piece =
  {
    name = 't';
    state = [| [| 'i'; 'i' |] |];
    row = 0;
    col = 0;
    rot = 0;
  }

let board_tests =
  [
    board_test "drop piece on empty 2x2 board" em_2x2
      (fun b ->
        drop test_piece b;
        b)
      [| [| ' '; ' ' |]; [| 'i'; 'i' |] |]
      (pp_board pp_string);
    board_test "update piece on empty 2x2 board" em_2x2
      (fun b ->
        update_board test_piece b;
        b)
      [| [| 'i'; 'i' |]; [| ' '; ' ' |] |]
      (pp_board pp_string);
    board_test "check_valid with 3x3 board"
      [|
        [| ' '; ' '; ' ' |]; [| ' '; ' '; ' ' |]; [| 'o'; ' '; 'o' |];
      |]
      (fun b -> check_valid test_piece b)
      true string_of_bool;
    ( "check_valid with column out of bounds" >:: fun _ ->
      assert_equal false
        (let b =
           [|
             [| ' '; ' '; ' ' |];
             [| ' '; ' '; ' ' |];
             [| 'o'; ' '; 'o' |];
           |]
         in
         let t =
           {
             name = 't';
             state = [| [| 'l'; 'l'; ' ' |] |];
             col = -1;
             row = 0;
             rot = 0;
           }
         in
         check_valid t b)
        ~printer:string_of_bool );
    ( "check_valid with row out of bounds" >:: fun _ ->
      assert_equal false
        (let b =
           [|
             [| ' '; ' '; ' ' |];
             [| ' '; ' '; ' ' |];
             [| 'o'; ' '; 'o' |];
           |]
         in
         let t =
           {
             name = 't';
             state = [| [| 'l'; 'l'; ' ' |] |];
             col = 0;
             row = 4;
             rot = 0;
           }
         in
         check_valid t b)
        ~printer:string_of_bool );
    ( "cleared_rows empty board" >:: fun _ ->
      assert_equal 1
        (let b = [| [| ' '; ' ' |]; [| ' '; ' ' |] |] in
         drop
           {
             name = 't';
             state = [| [| 'l'; 'l' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         cleared_rows b)
        ~printer:string_of_int );
    ( "cleared_rows bottom row filled" >:: fun _ ->
      assert_equal 2
        (let b = [| [| ' '; ' ' |]; [| 'o'; 'o' |] |] in
         drop
           {
             name = 's';
             state = [| [| 'l'; 'l' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         cleared_rows b)
        ~printer:string_of_int );
    ( "cleared_rows bottom row filled no drop" >:: fun _ ->
      assert_equal 1
        (let b = [| [| ' '; ' ' |]; [| 'o'; 'o' |] |] in
         cleared_rows b)
        ~printer:string_of_int );
    ( "cleared_rows both rows filled no drop" >:: fun _ ->
      assert_equal 2
        (let b = [| [| 'i'; 'i' |]; [| 'o'; 'o' |] |] in
         cleared_rows b)
        ~printer:string_of_int );
    ( "cleared_rows only left side filled" >:: fun _ ->
      assert_equal 0
        (let b = [| [| ' '; ' ' |]; [| 'o'; ' ' |] |] in
         drop
           {
             name = 's';
             state = [| [| 'l'; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         cleared_rows b)
        ~printer:string_of_int );
    ( "cleared_rows only right side filled" >:: fun _ ->
      assert_equal 0
        (let b = [| [| ' '; ' ' |]; [| ' '; 'o' |] |] in
         drop
           {
             name = 's';
             state = [| [| ' '; 'l' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         cleared_rows b)
        ~printer:string_of_int );
    ( "cleared_rows row gets filled" >:: fun _ ->
      assert_equal 1
        (let b = [| [| ' '; ' ' |]; [| ' '; 'o' |] |] in
         drop
           {
             name = 's';
             state = [| [| 'l'; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         cleared_rows b)
        ~printer:string_of_int );
    ( "cleared_rows 3x3" >:: fun _ ->
      assert_equal 1
        (let b =
           [|
             [| ' '; ' '; ' ' |];
             [| ' '; ' '; ' ' |];
             [| 'o'; ' '; 'o' |];
           |]
         in
         drop
           {
             name = 's';
             state = [| [| 'o'; ' ' |] |];
             col = 1;
             row = 0;
             rot = 0;
           }
           b;
         cleared_rows b)
        ~printer:string_of_int );
    ( "clear_lines empty board" >:: fun _ ->
      assert_equal true
        (let b = [| [| ' '; ' ' |]; [| ' '; ' ' |] |] in
         drop
           {
             name = 't';
             state = [| [| 'l'; 'l' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         clear_lines b)
        ~printer:string_of_bool );
    ( "clear_lines bottom row filled" >:: fun _ ->
      assert_equal true
        (let b = [| [| ' '; ' ' |]; [| 'o'; 'o' |] |] in
         drop
           {
             name = 's';
             state = [| [| 'l'; 'l' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         clear_lines b)
        ~printer:string_of_bool );
    ( "clear_lines only left side filled" >:: fun _ ->
      assert_equal false
        (let b = [| [| ' '; ' ' |]; [| 'o'; ' ' |] |] in
         drop
           {
             name = 's';
             state = [| [| 'l'; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         clear_lines b)
        ~printer:string_of_bool );
    ( "clear_lines 3x3" >:: fun _ ->
      assert_equal true
        (let b =
           [|
             [| ' '; ' '; ' ' |];
             [| ' '; ' '; ' ' |];
             [| 'o'; ' '; 'o' |];
           |]
         in
         drop
           {
             name = 's';
             state = [| [| 'o'; ' ' |] |];
             col = 1;
             row = 0;
             rot = 0;
           }
           b;
         clear_lines b)
        ~printer:string_of_bool );
    ( "clear_lines 4x4" >:: fun _ ->
      let b =
        [|
          [| ' '; ' '; ' '; ' ' |];
          [| ' '; ' '; ' '; ' ' |];
          [| 'l'; ' '; 'o'; 'o' |];
          [| 'l'; ' '; 'o'; 'o' |];
        |]
      in
      assert_equal true
        (drop
           {
             name = 's';
             state =
               [| [| ' '; 'i'; ' '; ' ' |]; [| ' '; 'i'; ' '; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b;
         clear_lines b)
        ~printer:string_of_bool );
    ( "clear_lines 4x4 make sure board is updated" >:: fun _ ->
      let expected_b =
        [|
          [| ' '; ' '; ' '; ' ' |];
          [| ' '; ' '; ' '; ' ' |];
          [| ' '; ' '; ' '; ' ' |];
          [| ' '; ' '; ' '; ' ' |];
        |]
      in
      let original_b =
        [|
          [| ' '; ' '; ' '; ' ' |];
          [| ' '; ' '; ' '; ' ' |];
          [| 'l'; ' '; 'o'; 'o' |];
          [| 'l'; ' '; 'o'; 'o' |];
        |]
      in
      assert_equal expected_b
        (drop
           {
             name = 's';
             state =
               [| [| ' '; 'o'; ' '; ' ' |]; [| ' '; 'o'; ' '; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           original_b;
         ignore (clear_lines original_b);
         original_b)
        ~printer:(pp_board pp_string) );
    ( "lowest_possible 2x2" >:: fun _ ->
      assert_equal
        {
          name = 't';
          state = [| [| 'l'; 'l' |] |];
          col = 0;
          row = 0;
          rot = 0;
        }
        (let b = [| [| ' '; ' ' |]; [| 'o'; ' ' |] |] in
         let t =
           {
             name = 't';
             state = [| [| 'l'; 'l' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
         in
         get_lowest_possible t b) );
    ( "lowest_possible middle row" >:: fun _ ->
      assert_equal
        {
          name = 't';
          state = [| [| 'l'; 'l'; ' ' |] |];
          col = 0;
          row = 1;
          rot = 0;
        }
        (let b =
           [|
             [| ' '; ' '; ' ' |];
             [| ' '; ' '; ' ' |];
             [| 'o'; ' '; 'o' |];
           |]
         in
         let t =
           {
             name = 't';
             state = [| [| 'l'; 'l'; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
         in
         get_lowest_possible t b) );
    ( "lowest_possible low row" >:: fun _ ->
      assert_equal
        {
          name = 't';
          state = [| [| 'l'; 'l'; ' ' |] |];
          col = 0;
          row = 2;
          rot = 0;
        }
        (let b =
           [|
             [| ' '; ' '; ' ' |];
             [| ' '; ' '; ' ' |];
             [| ' '; ' '; ' ' |];
           |]
         in
         let t =
           {
             name = 't';
             state = [| [| 'l'; 'l'; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
         in
         get_lowest_possible t b) );
    ( "lowest_possible high row" >:: fun _ ->
      assert_equal
        {
          name = 't';
          state = [| [| 'l'; 'l'; ' ' |] |];
          col = 0;
          row = 0;
          rot = 0;
        }
        (let b =
           [|
             [| ' '; ' '; ' ' |];
             [| 'o'; ' '; 'o' |];
             [| 'o'; ' '; 'o' |];
           |]
         in
         let t =
           {
             name = 't';
             state = [| [| 'l'; 'l'; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
         in
         get_lowest_possible t b) );
  ]

(*returns the state of a tetromino piece*)
let get_state piece = piece.state

let get_row piece = piece.row

let get_col piece = piece.col

let tetromino_tests =
  [
    ( "rotate o piece left" >:: fun _ ->
      assert_equal o_piece.state
        (rotate_left o_piece |> get_state)
        ~printer:(pp_board pp_string) );
    ( "rotate i piece right" >:: fun _ ->
      assert_equal
        [|
          [| ' '; ' '; ' '; ' '; ' ' |];
          [| ' '; ' '; 'i'; ' '; ' ' |];
          [| ' '; ' '; 'i'; ' '; ' ' |];
          [| ' '; ' '; 'i'; ' '; ' ' |];
          [| ' '; ' '; 'i'; ' '; ' ' |];
        |]
        (rotate_right i_piece |> get_state)
        ~printer:(pp_board pp_string) );
    ( "rotate t piece left" >:: fun _ ->
      assert_equal
        [|
          [| ' '; 't'; ' ' |]; [| 't'; 't'; ' ' |]; [| ' '; 't'; ' ' |];
        |]
        (rotate_left t_Piece |> get_state)
        ~printer:(pp_board pp_string) );
    ( "rotate i piece left and then right" >:: fun _ ->
      assert_equal i_piece.state
        (rotate_left i_piece |> rotate_right |> get_state)
        ~printer:(pp_board pp_string) );
    ( "rotate s piece right and then left" >:: fun _ ->
      assert_equal s_piece.state
        (rotate_left s_piece |> rotate_right |> get_state)
        ~printer:(pp_board pp_string) );
    ( "move i piece down" >:: fun _ ->
      assert_equal 0
        (move_down i_piece |> get_row)
        ~printer:string_of_int );
    ( "move i piece down twice" >:: fun _ ->
      assert_equal 1
        (move_down i_piece |> move_down |> get_row)
        ~printer:string_of_int );
    ( "move o piece left" >:: fun _ ->
      assert_equal 3
        (move_left o_piece |> get_col)
        ~printer:string_of_int );
    ( "move t piece right" >:: fun _ ->
      assert_equal 4
        (move_right t_Piece |> get_col)
        ~printer:string_of_int );
    ( "move t piece left and then right" >:: fun _ ->
      assert_equal 3
        (move_right t_Piece |> move_left |> get_col)
        ~printer:string_of_int );
    ( "match name i to the right piece" >:: fun _ ->
      assert_equal i_piece (match_name_to_default 'i') );
    ( "match name s to the right piece" >:: fun _ ->
      assert_equal s_piece (match_name_to_default 's') );
    ( "match name o to the right piece" >:: fun _ ->
      assert_equal o_piece (match_name_to_default 'o') );
    ( "match name t to the right piece" >:: fun _ ->
      assert_equal t_Piece (match_name_to_default 't') );
    ( "match name z to the right piece" >:: fun _ ->
      assert_equal z_piece (match_name_to_default 'z') );
  ]

let leaderboard_tests =
  [
    ( "compare_scores" >:: fun _ ->
      assert_equal 300 (compare_scores ("noah", 500) ("richard", 200))
    );
    ( "add score forward" >:: fun _ ->
      assert_equal
        [ ("noah r", 1200); ("noah s", 900) ]
        (add_score ("noah r", 1200)
           [ ("noah s", 900); ("richard", 200) ])
        ~printer:(pp_list pp_score) );
    ( "add score reverse" >:: fun _ ->
      assert_equal
        [ ("noah s", 900); ("noah r", 500) ]
        (add_score ("noah r", 500)
           [ ("noah s", 900); ("richard", 200) ])
        ~printer:(pp_list pp_score) );
    ( "add score same score" >:: fun _ ->
      assert_equal
        [ ("noah s", 1200); ("noah s", 900) ]
        (add_score ("noah s", 1200)
           [ ("noah s", 900); ("richard", 200) ])
        ~printer:(pp_list pp_score) );
    ( "add score one element" >:: fun _ ->
      assert_equal [ ("noah s", 1200) ]
        (add_score ("noah s", 1200) [ ("noah s", 900) ])
        ~printer:(pp_list pp_score) );
  ]

let bot_tests =
  [
    ( "landing_height" >:: fun _ ->
      assert_equal 0.5
        (let b = [| [| ' '; ' ' |]; [| ' '; ' ' |] |] in
         landing_height
           {
             name = 't';
             state = [| [| 'l'; 'l' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b)
        ~printer:string_of_float );
    ( "landing_height" >:: fun _ ->
      assert_equal 1.
        (let b = [| [| ' '; ' ' |]; [| ' '; 'l' |] |] in
         landing_height
           {
             name = 't';
             state = [| [| 'l'; 'l' |]; [| 'l'; ' ' |] |];
             col = 0;
             row = 0;
             rot = 0;
           }
           b)
        ~printer:string_of_float );
    ( "row_transitions" >:: fun _ ->
      assert_equal 8.
        (let b =
           [| [| 'l'; ' '; 'l'; ' '; 'l'; ' '; 'l'; ' '; 'l' |] |]
         in
         row_transitions b)
        ~printer:string_of_float );
    ( "row_transitions" >:: fun _ ->
      assert_equal 8.
        (let b =
           [|
             [| 'l' |];
             [| ' ' |];
             [| 'l' |];
             [| ' ' |];
             [| 'l' |];
             [| ' ' |];
             [| 'l' |];
             [| ' ' |];
             [| 'l' |];
           |]
         in
         column_transitions b)
        ~printer:string_of_float );
    ( "get_well_sum" >:: fun _ ->
      assert_equal 3.
        (let b =
           [|
             [| 'l'; 'l'; ' '; 'l'; 'l' |];
             [| 'l'; ' '; ' '; 'l'; 'l' |];
             [| 'l'; ' '; ' '; ' '; ' ' |];
           |]
         in
         get_well_sum b)
        ~printer:string_of_float );
    ( "get_well_sum" >:: fun _ ->
      assert_equal 6.
        (let b =
           [|
             [| 'l'; 'l'; ' '; 'l'; 'l' |];
             [| 'l'; 'l'; ' '; 'l'; 'l' |];
             [| 'l'; 'l'; ' '; 'l'; 'l' |];
           |]
         in
         get_well_sum b)
        ~printer:string_of_float );
    ( "holes" >:: fun _ ->
      assert_equal 2.
        (let b = [| [| 'l'; 'l' |]; [| ' '; ' ' |] |] in
         holes b)
        ~printer:string_of_float );
  ]

let suite =
  "test suite for ocamtris"
  >::: List.flatten
         [ board_tests; tetromino_tests; leaderboard_tests; bot_tests ]

let _ = run_test_tt_main suite
