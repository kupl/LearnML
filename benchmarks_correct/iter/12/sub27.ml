exception ERR_invalid_iter_val

let rec iter(n, f) =
	if (n == 0) then (fun x -> x)
	else if (n == 1) then f
	else if (n > 1) then (function x -> (iter(n-1 , f) (f x)))
	else raise ERR_invalid_iter_val
