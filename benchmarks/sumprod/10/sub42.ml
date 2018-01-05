let sumprod (matrix, n, k) =
	let rec pi i_ j =
		if j > k
			then 1.0
			else (matrix (i_, j)) *. (pi i_ (j+1)) in

	let rec sum i =
		if i > n 
			then 0.0
			else (pi i 1) +. (sum (i+1)) in

	sum 1

