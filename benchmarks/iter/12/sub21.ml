(*2009-11718 1-3*)

let rec iter (n, f) x =
	if n=0 then x
	else (f (iter (n-1, f) x))
