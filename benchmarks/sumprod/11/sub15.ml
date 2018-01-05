(*
2008-12155
±èÂùÈ£
*)

let sumprod(matrix, n, k) =
	let rec prod(j, k, i) =
		if j>k or j<=0 then
			raise (Invalid_argument "Bounds are not appropriate.")
		else if j<k then
			prod(j+1, k, i) *. matrix(i, j)
		else
			matrix(i, j)
	in
	let rec sum(i, n, k) =
		if i>n or i<=0 then
			raise (Invalid_argument "Bounds are not appropriate.")
		else if i<n then
			sum(i+1, n, k) +. prod(1, k, i)
		else
			prod(1, k, i)
	in
	sum(1, n, k)