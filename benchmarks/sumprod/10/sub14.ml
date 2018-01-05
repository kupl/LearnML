let rec muls (f, i, j) =
	if j = 1 then  f(i, j)
		else muls (f, i, j-1) *. f(i, j)

let rec sumprod (f, n, k) =
	if n = 1 then muls (f, n, k)
		else sumprod (f, n-1, k) +. muls (f, n, k)

