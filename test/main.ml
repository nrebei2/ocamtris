open OUnit2
open Game
open Tetromino
open Board

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
         drop { state = [| [| 'i'; 'o' |] |]; row = 0; col = 0 } b;
         drop { state = [| [| 'i'; ' ' |] |]; row = 0; col = 0 } b;
         b)
        ~printer:(pp_board pp_string) );
    ( "update piece on board" >:: fun _ ->
      assert_equal
        [| [| 'i'; ' ' |]; [| ' '; ' ' |] |]
        (let b = [| [| ' '; ' ' |]; [| ' '; ' ' |] |] in
         update_board { state = [| [| 'i' |] |]; row = 0; col = 0 } b;
         b)
        ~printer:(pp_board pp_string) );
    ( "valid drop" >:: fun _ ->
      assert_equal true
        (check_valid i_piece board)
        ~printer:string_of_bool );
    ( "clear_lines" >:: fun _ ->
      assert_equal
        [| [| ' '; ' ' |]; [| 'i'; ' ' |] |]
        (drop
           {
             state =
               [|
                 [| 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; 'l'; ' ' |];
               |];
             col = 0;
             row = 0;
           }
           board;
         clear_lines board;
         board)
        ~printer:(pp_board pp_string) );
  ]

let tetromino_tests = []

let suite =
  "test suite for ocamtris"
  >::: List.flatten [ board_tests; tetromino_tests ]

let _ = run_test_tt_main suite
