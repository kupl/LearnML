(* KIHWAN KANG HW01-1 *)

let rec sigma (i, n, f) =
	if i > n 
	then 0
	else f i + sigma ((i + 1), n, f)
