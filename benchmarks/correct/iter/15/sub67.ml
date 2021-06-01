let rec iter (n, (f : int -> int)) x =
	if n = 0 then x
	else iter (n - 1, f) (f x);;
