let rec iter (n, f) = function x ->
	if n <= 0 then x else iter(n - 1, f) (f x)
