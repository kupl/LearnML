let sumprod (matrix, n, k) =
	let rec product n k =
		if (k = 1) then matrix(n,1)
		else matrix(n,k) *. product n (k-1)
	in
	let rec sum n k =
		if (n = 1) then product 1 k
		else product n k +. sum (n-1) k
	in sum n k