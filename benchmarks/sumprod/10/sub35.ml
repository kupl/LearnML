exception Error of string
let sumprod (matrix_, n_, k_) =
	let rec product (matrix, n, k) =
		if k=1 then matrix (n ,k)
		else if k>1 then (matrix (n,k))*.(product (matrix , n, k-1))
		else raise (Error "k is not a positive integer") in
	let rec sum (matrix, n, k) =
		if n=1 then product(matrix,n,k)
		else if n>1 then product(matrix,n,k)+.sum(matrix,n-1,k)
		else raise (Error "n is not a postive integer") in
	sum(matrix_,n_,k_)
