let rec iter(n, f) =
	if n<0 then invalid_arg "iter"
	else if n=0 then function x -> x
	else function x-> iter (n-1, f) (f x)