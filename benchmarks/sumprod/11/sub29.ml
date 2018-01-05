exception InvalidInput		(* Exception : n, k가 1보다 작을 경우 *)

let rec sumprod (mat, n, k) =

	(* Production part *)
	let rec sumprod_tmp i j =
		if j<1 then raise InvalidInput
		else if j=1 then (mat (i, 1))
		else (mat (i, j)) * (sumprod_tmp i (j-1))
	in
	
	(* Summation part *)
	if n<1 then raise InvalidInput
	else if n=1 then sumprod_tmp 1 k
	else (sumprod_tmp n k) + (sumprod (mat, n-1, k))
