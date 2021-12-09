(* What we did: stuff *)

open OUnit2
open Game
open Tetromino
open Board
open Bot

(** Adopted from A2: [pp_string s] pretty-prints string [s]. *)
let pp_string s = s

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

let tetromino_tests = []

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
  >::: List.flatten [ board_tests; tetromino_tests; bot_tests ]

let _ = run_test_tt_main suite
