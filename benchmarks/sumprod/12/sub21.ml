(*2009-11718 1-4*)

let rec sumprod(matrix, n, k)=
	let rec prod (mat, i, j) =
		if j<1 then raise (Invalid_argument "error")
		else if j>1 then
		mat (i, j) *. prod (mat, i, (j-1))
		else
		mat (i, j) 	in

	if n<1 then raise (Invalid_argument "error")
	else if n>1 then
	prod (matrix, n, k) +. sumprod (matrix, (n-1), k)
	else 
	prod (matrix, n, k)

