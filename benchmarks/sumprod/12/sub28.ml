let sumprod (matrix, n, k) =
	let rec pi (a, b, f) =
		if a == b then (f a)
		else (f a) *. (pi ((a+1), b, f))
		in

	let rec sumprod_helper (matrix, n1, n2, k) =
		if n1 > n2 then 0.
		else pi (1, k, (fun x -> matrix(n1, x))) +. sumprod_helper(matrix, n1+1, n2, k)
		in
	
	sumprod_helper (matrix, 1, n, k);;
