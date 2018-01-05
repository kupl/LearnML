(* CSE/ 2004-11920 / Yeseong Kim/ Prob 2*)
exception Error of string

let rec sumprod (matrix, n, k) =
	let rec prod(matrix, n, k) = 
		if (k = 1) then matrix(n,k)
		else matrix(n,k) *. prod(matrix, n, k-1)
	in
	if ((n <= 0) || (k <= 0)) then raise (Error "n, k must be larger than 0")
	else if (n = 1) then prod(matrix, n, k)
	else prod(matrix, n, k) +. sumprod(matrix, n-1, k)
