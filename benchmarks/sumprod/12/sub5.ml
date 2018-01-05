let sumprod (matrix, n, k) =
	let rec prod (mat, n, l, k) =
		if l=k then mat(n,k)
			else (mat(n,l)) *. (prod (mat, n, l+1,k))
	in
	let rec sum (mat, r,n) =
		if r=n then prod (mat, n, 1, k)
			else (prod (mat, r, 1, k)) +. sum(mat, r+1, n)
	in
	sum(matrix, 1,n)