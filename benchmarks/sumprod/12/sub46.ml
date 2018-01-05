exception Out_of_bound

let rec sumprod (mat, (n:int), (k:int)) = 
	let rec pi mat (n:int) (k:int) =
		if k <= 0 then raise Out_of_bound
		else if k = 1 then (mat (n, k))
		else (mat (n, k)) *. (pi mat n (k-1))
	in
	
	let rec sigma (n:int) (k:int) = 
		if n <= 0 then raise Out_of_bound
		else if n = 1 then (pi mat n k)
		else (pi mat n k) +. (sigma (n-1) k)
	in

	(sigma n k)
