(* 2006-11377 hw1-1 *)

let rec sigma func start finish =
	if start > finish then
		0
	else
		(func start) + sigma func (start+1) finish