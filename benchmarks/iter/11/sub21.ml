(* 2009-11674 ����� HW1-2 *)

let rec iter(n,f) x =
	if (n=0) then x
	else iter((n-1),f) (f x)
