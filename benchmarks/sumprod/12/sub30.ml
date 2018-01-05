let rec sumprod(matrix, n, k) =
	
	let rec prod (matrix, n1, k) =
		if k == 1 then matrix(n1, 1)
		else matrix(n1, k) *. prod(matrix, n1, k-1) in
	
	if n == 1 then prod(matrix, 1, k)
	else prod(matrix, n, k) +. sumprod(matrix, n-1, k)

