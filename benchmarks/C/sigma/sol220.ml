(* KIHWAN KANG HW01-1 *)

let rec sigma f i n =
	if i > n 
	then 0
	else f i + sigma f (i+1) n
