open Yojson.Basic.Util
open Graphics

type score = string * int

type leaderboard_scores = score list

type score_json = {
  name : string;
  score : int;
}

let score_of_json json : string * int =
  let name = json |> member "name" |> to_string in
  let score = json |> member "score" |> to_int in
  (name, score)

let scores_of_json json =
  json |> member "scores" |> to_list |> List.map score_of_json

let from_json_file filename : leaderboard_scores =
  let json_contents = Yojson.Basic.from_file filename in
  try scores_of_json json_contents
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let score_to_json (score : score) : Yojson.t =
  let player_name, player_score = score in
  `Assoc [ ("name", `String player_name); ("score", `Int player_score) ]

let score_lst_to_json (scores : score list) =
  `List (List.map score_to_json scores)

let to_json (scores : leaderboard_scores) : Yojson.t =
  `Assoc [ ("scores", score_lst_to_json scores) ]

let save_leaderboard_file
    (scores : leaderboard_scores)
    (filename : string) =
  Yojson.to_file filename (to_json scores)

(* let should_add_score (new_score : score) (scores :
   leaderboard_scores) : bool = if snd new_score < (scores |> List.rev
   |> List.hd |> snd) then false else true *)

(* let rec player_exists (new_score : score) (scores :
   leaderboard_scores) : bool = match scores with | [] -> false | (name,
   _) :: t -> if name = fst new_score then true else player_exists
   new_score t *)

let compare_scores x y = snd x - snd y

let add_score (new_score : string * int) (scores : (string * int) list)
    =
  [ new_score ] @ scores
  |> List.sort compare_scores
  |> List.tl |> List.rev

let display_leaderboard vertical_pos =
  resize_window 600 800;
  moveto 100 vertical_pos;
  set_color black;
  draw_string "leaderboard:";
  let _, text_height = text_size "leaderboard:" in
  let rec draw_scores vertical_pos scores_lst = 
  match scores_lst with
  | [] -> ()
  | h :: t ->
      moveto 100 vertical_pos;
      let player_name, player_score = h in
      draw_string (player_name ^ ": " ^ string_of_int player_score);
      draw_scores (vertical_pos - text_height) t 
  in draw_scores (vertical_pos - text_height) (("assets" ^ Filename.dir_sep ^ "leaderboard.json") |> from_json_file)