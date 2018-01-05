
let rec prod(matrix, n, k) = 
	if k=1 then matrix(n,1)
	else if k>1 then  matrix(n,k) *. prod(matrix, n, k-1)
	else raise(Invalid_argument "sumproduct")

let rec sumprod(matrix, n, k) =
	if n=1 then prod(matrix, 1, k)
	else if n>1 then prod(matrix, n, k) +. sumprod(matrix, n-1, k)
	else raise(Invalid_argument"sumproduct")



