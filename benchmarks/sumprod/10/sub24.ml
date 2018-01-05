exception Error of string
let rec sumprod (matrix, n, k) = 
  	let rec prod (i, j, k) =
	if j = k then matrix (i, j) 
		else if j < k then matrix (i, j) *. prod(i, j+1, k)
		else raise (Error "invalid input!")
	in 
	let rec sum (a, b) = 
		if a = b then prod (a, 1, k)   
		else if a < b then prod (a, 1, k) +. sum(a+1, b)
		else raise (Error "invalid input!")
	in
	if n < 1 then raise(Error "invalid input")
	else if k < 1 then raise (Error "invalid input!")
	else sum(1, n);;
