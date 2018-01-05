let sumprod(matrix,n,k) =
	let rec pi def j =
		if j > k then 1.0
		else (matrix(def,j)) *. (pi def (j+1))
	in
	let rec prodmodule i =
		if i > n then 0.0
		else (pi i 1) +. (prodmodule (i+1))
	in
	prodmodule 1

