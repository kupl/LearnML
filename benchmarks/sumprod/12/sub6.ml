let rec sumprod(mat,n,k) = 
	let rec pi(mat,n,k) = 
		if k<1 then 1.
		else mat(n,k) *. pi(mat,n,k-1)
	in
	if n<1 then 0.
	else pi(mat,n,k) +. sumprod(mat,n-1,k)
