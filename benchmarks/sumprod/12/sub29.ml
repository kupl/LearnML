let rec sumprod (matrix, n, k) = 
	let rec sum (i, res) =
		let rec pro (j, res_temp) =
			if( j <= k ) then matrix(i, j) *. pro(j+1, res_temp) 
			else res_temp
		in
		if ( i <= n ) then sum(i+1, res +. pro(1, 1.0)) else res
	in
	sum (1, 0.0)
