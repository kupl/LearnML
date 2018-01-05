let rec sumprod (mat, n, k) =
	let rec prod (mat, n, k) =
		if k=1 then (mat (n, k))
		else (mat (n, k))*.(prod (mat, n, k-1)) in
	if n=1 then prod (mat, n, k)
	else (prod (mat, n, k)) +. (sumprod (mat, n-1, k))

