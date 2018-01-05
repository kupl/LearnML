let rec product(matrix, n, k) =
	if k > 0 then product(matrix, n, k - 1) *. matrix(n, k)
	else 1.

let rec sumprod(matrix, n, k) = 
	if n > 0 then sumprod(matrix, n - 1, k) +. product(matrix, n, k)
	else 0.
