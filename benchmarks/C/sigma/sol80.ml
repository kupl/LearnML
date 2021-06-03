(* 2009-11824 Jieun-Jeong HW1-1 *)

let rec sigma ftn a b =
	if a > b then raise (Invalid_argument "a is bigger than b")
	else if a == b then ftn a
	else (ftn a)+(sigma ftn (a+1) b)
