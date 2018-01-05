let sumprod : (int*int->float) * int * int -> float = fun ( matrix, n, k) ->
	let rec prod' = fun ( matrix, n, k) ->
		if k > 1 then prod' (matrix, n, k-1) *. (matrix (n, k))
		else if k = 1 then matrix (n, k)
		else raise ( Invalid_argument "k must be >=1")
	in
		let rec sum' = fun ( matrix, n, k ) ->
			if n > 1 then sum' (matrix, n-1, k) +. prod' (matrix, n, k)
			else if n = 1 then prod' (matrix, n, k)
			else raise ( Invalid_argument "n must be >=1")
		in
			sum' (matrix, n , k)