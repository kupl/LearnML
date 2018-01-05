exception Error of string

let rec prod (matrix, n, k) =
	if (n<=0) || (k<=0) then raise (Error "n, k nonpositive")
	else if k=1 then matrix(n,k)
	else matrix(n,k) *. prod (matrix, n, k-1)

let rec sumprod (matrix, n, k) =
	if (n<=0) || (k<=0) then raise (Error "n, k nonpositive")
	else if (n=1) then prod(matrix, n, k)
	else prod(matrix, n, k) +. sumprod(matrix,n-1,k)
