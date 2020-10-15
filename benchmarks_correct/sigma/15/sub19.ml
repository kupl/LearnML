(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #2 *)
let rec sigma f a b  =
		if a>b then 0
		else f b + sigma f a (b-1)
