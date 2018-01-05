
	
let rec sumprod(matrix, n, k) =
	let rec getprod(i, j) =
		if j=1 then matrix (i, j)
		else matrix (i, j) *. getprod(i, j-1)
	in

	if n>0 then getprod(n, k) +. sumprod(matrix, n-1, k)
	else 0.0
