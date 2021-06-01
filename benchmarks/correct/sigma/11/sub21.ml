(* 컴퓨터공학부/2009-11679/김정명/1 *)

let rec sigma f a b = 
	if a > b then raise (Invalid_argument "sigma")
	else if a = b then f a
	else sigma f (a+1) b + f a
