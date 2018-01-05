exception Error of string

let rec sumprod (matrix, n, k) =
	let rec prod (matrix, i, k) =
		if k = 0 then 1.0
		else prod(matrix, i, k-1) *. matrix(i, k)
	in
	
	if n <= 0 then
		raise (Error "n <= 0")
	else if k <= 0 then
		raise (Error "k <= 0")
    else if n = 1 then
        prod(matrix, 1, k)
	else
        sumprod (matrix, n-1, k) +. prod(matrix, n, k)