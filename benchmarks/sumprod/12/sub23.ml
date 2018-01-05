let rec sumprod (mat, n, k) =
	
	let rec sumprodTmp (i, j) =
		if(j<1) then 0.
		else if(j==1) then mat(i, 1)
		else mat(i, j) *. sumprodTmp(i, j-1)
	in

	if(n<1) then 0.
	else if(n==1) then sumprodTmp(1, k)
	else sumprodTmp(n, k) +. sumprod(mat, n-1, k)
