(* 2008-11874 EXERCISE 3 *)

let rec iter(n,f) x =
	if n=0 then x
	else iter(n-1,f) (f x)