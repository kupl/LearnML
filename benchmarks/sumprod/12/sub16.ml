(* ex4 *)
let sumprod (matrix, n, k) = 
	let rec loopMul (i, j) = 
		if j > k then 1.0
		else (matrix (i, j)) *. (loopMul (i, j + 1)) in

	let rec loopAdd (i) = 
		if i > n then 0.
		else (loopMul (i, 1)) +. (loopAdd (i + 1)) in

	loopAdd (1)
	