let rec sumprod(matrix, n, k) =
	let rec rowprod(matrix, n, k) =
		if k == 0 then 1.
		else rowprod(matrix, n, k-1)*.matrix(n,k)
	in
	if n == 0 then 0.
	else sumprod(matrix, n-1, k)+.rowprod(matrix, n, k)
