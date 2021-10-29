open Yojson.Basic.Util

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

let should_add_score (new_score : score) (scores : leaderboard_scores) :
    bool =
  if snd new_score < (scores |> List.rev |> List.hd |> snd) then false
  else true

let compare_scores x y = snd x - snd y

let add_score
    (new_score : string * int)
    (leaderboard : (string * int) list) =
  [ new_score ] @ leaderboard
  |> List.sort compare_scores
  |> List.tl |> List.rev
