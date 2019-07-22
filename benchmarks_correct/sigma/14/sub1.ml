(* 2006-11377 hw1-1 *)

let rec sigma (start, finish, func) =
	if start > finish then
		0
	else
		(func start) + sigma(start+1, finish, func)