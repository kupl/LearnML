let rec sumprod (matrix, n, k) =
	let rec sum a b f =
		if a > b then
			0.0
		else
			f a +. sum (a + 1) b f
	in
	let rec prod a b f =
		if a > b then
			1.0
		else
			f a *. prod (a + 1) b f
	in
	sum 1 n (fun x -> prod 1 k (fun y -> matrix (x, y)))
