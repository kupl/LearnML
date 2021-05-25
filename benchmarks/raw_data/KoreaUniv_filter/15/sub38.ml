(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
match lst with
|[] -> lst
|hd::tl ->
	(match pred hd with
	|true -> hd::filter pred tl
	|_ -> filter pred tl)
	