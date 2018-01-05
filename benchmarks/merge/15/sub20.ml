(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #1 *)
let rec merge (p : int list * int list) : int list =
		match p with
		| ([], l) -> l
        | (l, []) -> l
        | (ha :: ta, hb :: tb) -> 
        	if ha>hb then ha :: merge (ta, (hb :: tb))
        	else hb :: merge ((ha :: ta), tb)
