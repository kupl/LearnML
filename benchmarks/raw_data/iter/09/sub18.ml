
exception Error of string
	
let iter (n, f) init=	
	let rec gen n=
	if n=0 then []
	else if n<0 then raise (Error "invalid arg")
	else 1::gen(n-1)
	in
	let rec iter_sub f iter_list result=
		match iter_list with
			[] -> result
			| h::t -> iter_sub f t (f result)
	in	
	iter_sub f (gen (n)) init
