let rec iter = fun (n, f) x ->
	if n > 0 then iter (n-1, f) (f x)
	else if n = 0 then x
	else raise (Invalid_argument "n must be >=0")