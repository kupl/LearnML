exception Error of string

let rec sumprod (matrix,n,k) =
	let rec prod j =
		if (j=1) then matrix(n,j)
		else matrix(n,j) *. prod (j-1)
	in
		if (n<1 or k<1) then raise (Error "Invalid argument")
		else if (n=1) then prod k
		else (prod k) +. sumprod(matrix,(n-1),k)