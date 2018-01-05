let rec sumprod ( mat, n, k) = 
	let rec pro ( mat, i, k) = 
	if k = 0 then 1.
	else (mat (i,k)) *.(pro (mat, i, k-1))
	in
	if n = 0 then 0.
	else (pro (mat, n, k)) +. (sumprod (mat, n-1, k))

