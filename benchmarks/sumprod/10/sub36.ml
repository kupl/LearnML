exception Error of string
type real = float
let rec sumprod (matrix, n, k) = 
	let rec pi (matrix, p, q) = 
		if ( q > 1 ) then
		(matrix (p, q)) *. (pi (matrix, p, (q - 1)))
		else
		matrix (p, 1)
	in
	if (n <= 0) or (k <= 0) then
	raise ( Error "invalid" )
	else if ( n > 1 ) then
	(pi (matrix, n , k)) +. (sumprod (matrix, (n - 1), k))
	else
	(pi (matrix, 1, k))
		

