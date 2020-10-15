let rec sigma f a b =
	if a > b then 0
	else
		if a == b
			then f a
			else (f a) + (sigma f (a+1) b)

(*
let func a = a

let _ = print_int (sigma (1, 5, func))
*)
