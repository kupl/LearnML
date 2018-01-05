let sumprod ( matrix, n, k ) =
	let value i j = matrix ( i, j ) in
	let rec sigma n	= if n < 1 then 0. else
		let rec pi k = if k < 1 then 1. else ( value n k ) *. ( pi (k-1) ) in
		( pi k ) +. ( sigma (n-1) ) in
	sigma n
(*
let mat (i, j) = 1

let _ = print_int (sumprod (mat, 3, 3))
*)
