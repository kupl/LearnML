let sumprod (matrix, n, k) =
	let rec prod i j =
		if j = k then matrix (i, j) 
		else matrix (i, j) *. (prod i (j+1))
	in
	let rec sum i =
		if i = 1 then (prod i 1)
		else (sum (i-1)) +. (prod i 1)
	in
	if n < 1 || k < 1 then raise (Invalid_argument "sumprod")
	else sum n
