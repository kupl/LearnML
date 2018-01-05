let rec realsigma a b h =
	if a = b then h b
	else (h a)+.(realsigma (a+1) b h);;

let rec pi a b f =
	if a = b then f b
	else (f a)*.(pi (a+1) b f);;

let rec sumprod matrix n k =
	let smallprod x =
		pi 1 k (matrix x) in
	realsigma 1 n smallprod;;

