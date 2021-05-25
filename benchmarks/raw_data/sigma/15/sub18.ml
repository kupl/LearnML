(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #2 *)
let rec sigma (t : int * int * (int -> int)) : int =
	match t with (a,b,f) ->
		if a>b then 0
		else f b + sigma(a, b-1, f)
