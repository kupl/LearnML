let rec sigma(a,b,f) = 
	if a>b then sigma(b,a,f) else
		if a=b then f(a) else f(a) +. sigma(a+1,b,f);;

let sumprod(matrix, n, k) =
	let rec prod(i,j) =
		if j=k then matrix(i,j) else matrix(i,j) *. prod(i,j+1)
	and prod_w(i) =
		prod(i,1)
	in sigma(1,n,prod_w);;
