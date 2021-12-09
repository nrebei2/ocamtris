(** Representation of leaderboard data.

    This module represents the data stored in leaderboard files,
    including the player names and scores. It handles loading of that
    data from JSON as well as querying the data. *)

type score = string * int
(** The type of an individual player (name and score). *)

type leaderboard_scores = score list
(** The type of the top 10 scores on the leaderboard. *)

val from_json_file : string -> leaderboard_scores
(** [from_json_file f] is the leaderboard that json file [f] represents.
    Requires: [f] is the filename of a valid JSON leaderboard
    representation. *)

val save_leaderboard_file : leaderboard_scores -> string -> unit
(** [save_leaderboard_file s f] exports scores [s] to filename [f]. *)

val add_score : score -> leaderboard_scores -> leaderboard_scores
(** Add a new score to the leaderboard, removing the lowest score *)

val compare_scores : score -> score -> int
(** [compare_scores s1 s2] returns the differencce between score [s1]
    and score [s2]. *)

val display_leaderboard : int -> unit
(** [display_leaderboard l] draws the leaderboard to the board. *)