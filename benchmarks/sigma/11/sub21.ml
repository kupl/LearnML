(* 컴퓨터공학부/2009-11679/김정명/1 *)

let rec sigma (a, b, f) = 
	if a > b then raise (Invalid_argument "sigma")
	else if a = b then f a
	else sigma(a+1, b, f) + f a
