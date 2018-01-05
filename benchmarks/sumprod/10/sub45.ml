let rec realsigma a b h =
	if a = b then h b
	else (h a)+.(realsigma (a+1) b h);;

let rec pi a b f c =
	if a = b then f (b, c)
	else (f (a,c))*.(pi (a+1) b f c);;

let sumprod (matrix, n, k) =
	let smallprod x =
		pi 1 k matrix x in
	realsigma 1 n smallprod;;

