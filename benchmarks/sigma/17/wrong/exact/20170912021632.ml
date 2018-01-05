(* 2014-15113 Kim Minji *)

let rec sigma a b (f: (int -> int)) = 
	if b < a then 0
	else if b = a then f a
	else (f b + sigma a (b-1) f)

