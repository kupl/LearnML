let sumprod (matrix, n, k) =
	let rec prod (i, j) =
		if j = k then matrix (i, j)
		else matrix (i, j) *. prod (i, j + 1)
	in
	let rec sum i =
		if i = n then prod (i, 1)
		else prod (i, 1) +. sum (i + 1)
	in
	sum 1