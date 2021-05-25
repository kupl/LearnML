(* 2009-11824 Jieun-Jeong HW1-2 *)

let rec iter (n, ftn) x=
	if n < 0 then raise (Invalid_argument "n is nonnegative")
	else if n == 0 then x
	else iter ((n-1), ftn) (ftn x)
