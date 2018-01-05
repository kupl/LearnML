let rec _sumprod (matrix, n, k) =
	if k=1 then matrix (n,k)
	else matrix (n,k) *. _sumprod(matrix, n, k-1)

let rec sumprod (matrix, n, k) = 
	if n=1 then
		_sumprod(matrix, n, k)
	else
		_sumprod(matrix, n, k) +. sumprod(matrix, n-1, k)
