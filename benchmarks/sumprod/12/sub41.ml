let rec sumprod (matrix,n,k) = 
	let rec mul i j =
		if j > k then 1.0
		else matrix(i,j) *. (mul i (j+1))
	in
	let rec sigma i = 
		if i > n then 0.0
		else (mul i 1) +. sigma (i+1)
	in
	sigma 1
