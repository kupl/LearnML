let rec pi(i, j, f) =
	if 1>j then 1.0
	else f(i, j) *. pi(i, j-1, f)

let rec sumprod(matrix, n, k) =
	if n<1 || k<1 then invalid_arg "invalid value!"
	else if n=1 then pi(1, k, matrix)
	else pi(n, k, matrix) +. sumprod(matrix, n-1, k)
