(*
2008-12155
±èÂùÈ£
*)

let rec iter(n, f) x =
	if n<0 then
		raise(Invalid_argument "Negative integers can't be treated.")
	else if n=0 then x
	else iter(n-1, f) (f x)
