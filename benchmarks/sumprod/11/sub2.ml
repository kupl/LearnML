
let rec sumprod (matrix, n, k)  =
	let rec prod(matrix, n, k) =
		if k < 1 then 1.0
		else prod(matrix, n, k-1) *. matrix(n,k)
	in
		if n < 1 then 0.0
		else sumprod (matrix, n-1,k) +.
			prod(matrix, n, k)
