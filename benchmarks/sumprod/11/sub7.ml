exception Error of int*int

let rec pi (matrix,n,k) =
	if k=1 then matrix (n,1)
	else if k>1 then  matrix (n,k) *. pi(matrix,n,(k-1))
	else raise (Error (n,k))

let rec sumprod (matrix,n,k) =
	if n=1 then pi(matrix,1,k)
	else if n>1 then pi(matrix,n,k) +. sumprod(matrix,(n-1),k)
	else raise (Error (n,k))

