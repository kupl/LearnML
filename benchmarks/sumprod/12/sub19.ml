(* HW1 exercise4 2009-11697 Kim HyunJoon *)
(* Sumprod *)

let rec sumprod : (int * int -> float) * int * int -> float = 
	fun (matrix, n, k) ->
	let rec prod (matrix, i, k) =
		if k < 1 then invalid_arg "k < 1"
		else if k = 1 then (matrix (i, k))
		else (matrix (i, k)) *. (prod (matrix, i, k-1))
	in
	if n < 1 then invalid_arg "n < 1"
	else if n = 1 then (prod (matrix, n, k))
	else (prod (matrix, n, k)) +. (sumprod (matrix, n-1, k))
