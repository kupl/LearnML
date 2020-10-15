
(* ex 1 *)

let rec sigma f a b =
	if a > b then raise (Invalid_argument "sigma") 
	else if a < b then 
		(f a) + sigma f (a+1) b
	else
		(f a)

