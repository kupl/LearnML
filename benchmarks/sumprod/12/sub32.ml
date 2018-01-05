let rec prod (matrix, n, k) =
	if k = 0
			then 1.
			else
					matrix(n,k) *. prod (matrix, n, k - 1)
			
let rec sumprod (matrix, n, k) =
	if n = 0
			then 0.
			else 
					sumprod (matrix, n - 1, k) +. prod (matrix, n, k)


