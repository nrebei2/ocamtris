open OUnit2
open Game
open Tetromino
open Board
open Bot

(** Adopted from A2 [pp_string s] pretty-prints string [s]. *)
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

let board_tests =
  [
    ( "drop piece on board" >:: fun _ ->
      assert_equal
        [| [| 'i'; ' ' |]; [| 'i'; 'o' |] |]
        (let b = [| [| ' '; ' ' |]; [| ' '; ' ' |] |] in
         drop
           {
             name = 't';
             state = [| [| 'i'; 'o' |] |];
             row = 0;
             col = 0;
           }
           b;
         drop
           {
             name = 't';
             state = [| [| 'i'; ' ' |] |];
             row = 0;
             col = 0;
           }
           b;
         b)
        ~printer:(pp_board pp_string) );
    ( "update piece on board" >:: fun _ ->
      assert_equal
        [| [| 'i'; ' ' |]; [| ' '; ' ' |] |]
        (let b = [| [| ' '; ' ' |]; [| ' '; ' ' |] |] in
         update_board
           { name = 't'; state = [| [| 'i' |] |]; row = 0; col = 0 }
           b;
         b)
        ~printer:(pp_board pp_string) );
    ( "valid drop" >:: fun _ ->
      assert_equal true
        (check_valid i_piece board)
        ~printer:string_of_bool );
    ( "clear_lines" >:: fun _ ->
      assert_equal true
        (let b = [| [| ' '; ' ' |]; [| ' '; ' ' |] |] in
         drop
           {
             name = 't';
             state = [| [| 'l'; 'l' |] |];
             col = 0;
             row = 0;
           }
           b;
         clear_lines b)
        ~printer:string_of_bool );
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
