
(* ex 1 *)

let rec sigma(a,b,f) =
	if a > b then raise (Invalid_argument "sigma") 
	else if a < b then 
		(f a) + sigma(a+1,b,f)
	else
		(f a)

